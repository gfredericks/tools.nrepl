(ns clojure.tools.nrepl.middleware-test
  (:require (clojure.tools.nrepl.middleware
              interruptible-eval
              load-file
              pr-values
              session)
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.generators :as gen'])
  (:use [clojure.tools.nrepl.middleware :as middleware]
        clojure.test))

; wanted to just use resolve to avoid the long var names, but
; it seems that unqualified resolves *don't work* within the context of a
; clojure-maven-plugin test execution?!?
(def ^{:private true} default-middlewares
  [#'clojure.tools.nrepl.middleware.session/add-stdin
   #'clojure.tools.nrepl.middleware.load-file/wrap-load-file
   #'clojure.tools.nrepl.middleware/wrap-describe
   #'clojure.tools.nrepl.middleware.session/session
   #'clojure.tools.nrepl.middleware.interruptible-eval/interruptible-eval])

(defn- wonky-resolve [s] (if (symbol? s) (resolve s) s))

(defn- indexed-stack
  [x]
  (->> x
    (map wonky-resolve)
    shuffle
    linearize-middleware-stack
    (map-indexed #(vector (if (var? %2)
                            (-> (#'middleware/var-name %2) symbol name symbol)
                            %2)
                          %))
    (into {})))

(deftest sanity
  (let [stack (indexed-stack default-middlewares)]
    (is (stack 'pr-values))
    (are [before after] (< (stack before) (stack after))
         'interruptible-eval 'wrap-load-file
         'interruptible-eval 'session
         'wrap-describe 'pr-values
         'interruptible-eval 'pr-values))

  (let [n ^{::middleware/descriptor
            {:expects #{"clone"} :requires #{}}} {:dummy :middleware2}
        m ^{::middleware/descriptor
            {:expects #{"eval"} :requires #{n #'clojure.tools.nrepl.middleware.pr-values/pr-values}}}
           {:dummy :middleware}
        q ^{::middleware/descriptor
            {:expects #{} :requires #{"describe" "eval"}}} {:dummy :middleware3}
        stack (indexed-stack (concat default-middlewares [m q n]))]
    ;(->> stack clojure.set/map-invert (into (sorted-map)) vals println)
    (are [before after] (< (stack before) (stack after))
         'interruptible-eval m
         m 'pr-values
         'session n
         q 'wrap-describe
         m n

         'interruptible-eval 'wrap-load-file
         'interruptible-eval 'session
         'wrap-describe 'pr-values
         'interruptible-eval 'pr-values)))

(deftest append-dependency-free-middleware
  (let [m ^{::middleware/descriptor
            {:expects #{} :requires #{}}} {:dummy :middleware}
        n {:dummy "This not-middleware is supposed to be sans-descriptor, don't panic!"}
        stack (->> (concat default-middlewares [m n])
                shuffle
                linearize-middleware-stack)]
    (is (= #{n m} (set (take-last 2 stack))))))

(deftest no-descriptor-warning
  (is (.contains
        (with-out-str
          (binding [*err* *out*]
            (indexed-stack (conj default-middlewares {:dummy :middleware}))))
        "No nREPL middleware descriptor in metadata of {:dummy :middleware}")))

(deftest NREPL-53-regression
  (is (= [0 1 2]
         (map :id
              (linearize-middleware-stack
               [^{::middleware/descriptor
                  {:expects #{} :requires #{"1"}}}
                {:id 0}

                ^{::middleware/descriptor
                  {:expects #{} :requires #{} :handles {"1" {}}}}
                {:id 1}

                ^{::middleware/descriptor
                  {:expects #{"1"} :requires #{}}}
                {:id 2}])))))

;;
;; Generative tests for middleware linearization
;;

(defn- swap
  [coll [i1 i2]]
  (assoc coll i2 (coll i1) i1 (coll i2)))

(defn
  gen-shuffle
  "Create a generator that generates random permutations of `coll`. Shrinks
  toward the original collection: `coll`."
  [coll]
  (let [index-gen (gen/choose 0 (dec (count coll)))]
    (gen/fmap (partial reduce swap coll)
              ;; a vector of swap instructions, with count between
              ;; zero and 2 * count. This means that the average number
              ;; of instructions is count, which should provide sufficient
              ;; (though perhaps not 'perfect') shuffling. This still gives us
              ;; nice, relatively quick shrinks.
              (gen/vector (gen/tuple index-gen index-gen) 0 (* 2 (count coll))))))

(def gen-middlewares
  (gen'/for [count gen/nat
             :let [mids (map (fn [id] {:id id}) (range count))]
             shuf'd (gen-shuffle (vec mids))]
    shuf'd))

(defn all-pairs
  [coll]
  (if (<= (count coll) 1)
    []
    (let [[x & xs] coll]
      (concat (map #(vector x %) xs)
              (all-pairs xs)))))

(defn gen-dependencies
  "Generates [middlewares-with-descriptors id-pairs] where the latter
  is a collection of [before-id after-id] of ids that must come in a
  particular order."
  [middlewares]
  (let [pairs (all-pairs (map :id middlewares))
        middlewares (mapv #(vary-meta % assoc-in [::middleware/descriptor :handles] {(str (:id %)) {}})
                          middlewares)
        id->index (zipmap (map :id middlewares) (range))]
    (gen'/for [flags (gen/vector (gen'/subset #{:expects :requires}) (count pairs))
               :let [pairs&flags (->> (map vector pairs flags)
                                      (mapcat (fn [[pair flags]]
                                                (map #(vector pair %) flags))))
                     mids-with-descriptors
                     (reduce (fn [middlewares [[former latter] expect-or-require]]
                               (case expect-or-require
                                 :expects (update-in middlewares [(id->index latter)]
                                                     vary-meta update-in [::middleware/descriptor :expects]
                                                     (fnil conj #{}) (str former))
                                 :requires (update-in middlewares [(id->index former)]
                                                      vary-meta update-in [::middleware/descriptor :requires]
                                                      (fnil conj #{}) (str latter))))
                             middlewares
                             pairs&flags)]
               reordered (gen-shuffle mids-with-descriptors)]
      [reordered (distinct (map first pairs&flags))])))

(defspec hey-ho 200
  (prop/for-all [[mids id-pairs] (gen/bind gen-middlewares gen-dependencies)]
    (let [linearized (map :id (linearize-middleware-stack mids))]
      (every? (fn [[prior later]]
                (->> linearized
                     (filter #{prior later})
                     (= [prior later])))
              id-pairs))))
