(ns toolshed.core
  "A small collection of useful functions and macros.
   Inspired by medley, potpuri, and flub.")

(defn- editable? [coll]
  #?(:clj  (instance? clojure.lang.IEditableCollection coll)
     :cljs (satisfies? cljs.core.IEditableCollection coll)))

(defn- reduce-map [f coll]
  (if (editable? coll)
    (persistent! (reduce-kv (f assoc!) (transient (empty coll)) coll))
    (reduce-kv (f assoc) (empty coll) coll)))

(defn map-kv [f coll]
  (reduce-map (fn [xf] (fn [m k v]
                         (let [[k-new v-new] (f k v)]
                           (xf m k-new v-new))))
              coll))

(defn map-keys
  "Map the keys of given associative collection using function."
  [f coll]
  (reduce-map (fn [xf] (fn [m k v]
                         (xf m (f k) v)))
              coll))

(defn map-vals
  "Map the values of given associative collection using function."
  [f coll]
  (reduce-map (fn [xf] (fn [m k v]
                         (xf m k (f v))))
              coll))

(comment
  (map-kv (fn [k v] [k (inc v)]) {:a 1 :b 2})
  (map-kv (fn [k v] [k (inc v)]) {:a 1 :b 2})
  (reduce-kv assoc {} {:a 1 :b 2}))

(defn map-from-to [f g xs]
  (->> xs
       (map (juxt f g))
       (into {})))

(defn map-from [f xs]
  (map-from-to f identity xs))

(defn map-to [f xs]
  (map-from-to identity f xs))

(comment
  (map-from-to inc dec [1 2 3 4 5])
  (map-from inc [1 2 3 4 5])
  (map-to dec [1 2 3 4 5]))

(defn split-by [pred xs]
  (reduce #(update %1 (if (pred %2) 0 1) (fnil conj []) %2)
          [nil nil] xs))

(comment
  (split-with even? (range 10)) ;; contrast with split-with
  (split-by even? (range 10)))

(defn join [sep xs]
  (butlast (interleave xs (repeat sep))))

(comment
  (join :. [1 2 3 4 5]))

#?(:clj
   (do
     (def rfc3339 "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

     (defn parse-format-date [date in-format out-format]
       (cond->> date
         in-format (.parse (new java.text.SimpleDateFormat in-format))
         out-format (.format (new java.text.SimpleDateFormat out-format))))

     (defn parse-date
       ([date]
        (parse-date date rfc3339))
       ([date in-format]
        (parse-format-date date in-format nil)))

     (defn format-date
       ([date]
        (format-date date rfc3339))
       ([date out-format]
        (parse-format-date date nil out-format)))

     (defmacro catchall [& body]
       `(try ~@body (catch Exception ~'_ nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from Worldsingles
;; https://github.com/worldsingles/commons/blob/master/src/ws/clojure/extensions.clj

(defmacro condp->
  "Takes an expression and a set of predicate/form pairs. Threads expr (via ->)
  through each form for which the corresponding predicate is true of expr.
  Note that, unlike cond branching, condp-> threading does not short circuit
  after the first true test expression."
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[pred step]] `(if (-> ~g ~pred) (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro condp->>
  "Takes an expression and a set of predicate/form pairs. Threads expr (via ->>)
  through each form for which the corresponding predicate is true of expr.
  Note that, unlike cond branching, condp->> threading does not short circuit
  after the first true test expression."
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[pred step]] `(if (->> ~g ~pred) (->> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro condq
  "Takes a unary predicate, and a set of clauses.
  Each clause can take the form of either:
  test-expr result-expr
  test-expr :>> result-fn
  Note :>> is an ordinary keyword.
  For each clause, (pred test-expr) is evaluated. If it returns
  logical true, the clause is a match. If a binary clause matches, the
  result-expr is returned, if a ternary clause matches, its result-fn,
  which must be a unary function, is called with the result of the
  predicate as its argument, the result of that call being the return
  value of condq. A single default expression can follow the clauses,
  and its value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an
  IllegalArgumentException is thrown."
  {:copyright "Rich Hickey, since this is a modified version of condp"}
  [pred & clauses]
  (let [gpred (gensym "pred__")
        emit (fn emit [pred args]
               (let [[[a b c :as clause] more]
                     (split-at (if (= :>> (second args)) 3 2) args)
                     n (count clause)]
                 (cond
                   (= 0 n) `(throw (IllegalArgumentException. (str "No matching clause: " ~pred)))
                   (= 1 n) a
                   (= 2 n) `(if (~pred ~a)
                              ~b
                              ~(emit pred more))
                   :else `(if-let [p# (~pred ~a)]
                            (~c p#)
                            ~(emit pred more)))))]
    `(let [~gpred ~pred]
       ~(emit gpred clauses))))

(defn dissoc-all
  "Given a map and a sequence of keys, dissoc them all."
  [m ks]
  (apply dissoc m ks))

(defn flip
  "Like partial except you supply everything but the first argument.
  Also like Haskell's flip for single arity call."
  ([f] (fn [b a] (f a b)))
  ([f b] (fn [a] (f a b)))
  ([f b c] (fn [a] (f a b c)))
  ([f b c d & more]
   (fn [a] (apply f a b c d more))))

(comment
  (java.util.Date.)
  (format-date (java.util.Date.)))
