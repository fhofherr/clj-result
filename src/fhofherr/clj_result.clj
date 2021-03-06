(ns fhofherr.clj-result
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as cs]))

(defn end?
  "Check if `v` should end a sequence of computations."
  [v]
  (and (map? v)
       (contains? v ::end)))

(def ^{:doc "Opposite of [[end?]]"} continue? (complement end?))

(defn value
  "Get the value of `v` if it signals the end of a sequence of
  computations or `v`."
  [v]
  (::end v v))

(defn end
  "End a sequence of computations with `v` as result."
  [v]
  {::end v})

(defmacro end-ex-info
  "Wraps `op` in a `try` and catch any `clojure.lang.ExceptionInfo` it
  may throw."
  [op]
  `(try
    ~op
    (catch clojure.lang.ExceptionInfo e#
      (end e#))))

(s/fdef end-ex-info
        :args (s/cat :op any?)
        :ret any?)

(defn continue
  "Apply the function `f` to `v` if `v` does not signal an ended sequence
  of computations.

  `f` is expected to have the type

      v -> w

  where `w` is a potentially updated value `w`."
  [v f]
  {:pre [(fn? f)]}
  (if (end? v)
    v
    (-> v value f)))

(defn- swap-pairs
  [xs]
  {:pre [(even? (count xs))]}
  (let [xf (comp (partition-all 2)
                 (mapcat (fn [[x y]] [y x])))]
    (sequence xf xs)))

(defn- emit-continue
  [[r s & xs] body]
  (if xs
    `(continue ~r (fn [~s] ~(emit-continue xs body)))
    `(continue ~r (fn [~s] ~@body))))

(defmacro attempt
  "Attempt all given operations and bind the values to the respective symbols.
  Return the result of evaluating `body`. Abort immediately if one operation
  returns a value signaling an end and return that value instead."
  [bindings & body]
  (let [swapped (swap-pairs bindings)]
    (emit-continue swapped body)))

(s/fdef attempt
        :args (s/cat :bindings ::cs/bindings
                     :body any?))

(defmacro attempt-as->
  "Binds `init-value` to `sym`. If `init-value` does not signal an ended
  sequence of computations it evaluates the forms until it exhausted all or one
  signals the computations to end. Returns the result of the last form, or
  the first value singaling an end."
  [init-value sym & forms]
  `(attempt [~sym ~init-value ~@(mapcat #(vector sym %) forms)] ~sym))

(s/fdef attempt-as->
        :args (s/cat :init-result any?
                     :sym ::cs/local-name
                     :forms (s/* any?)))

(defn map-v
  "Apply the function `f` to the value `v`."
  [f v]
  (if (end? v)
    (update-in v [::end] f)
    (f v)))

(defn map-e
  "Apply the function `f` to `v` if `v` is signals an ended computation."
  [f v]
  (if (end? v)
    (map-v f v)
    v))

(defn map-s
  "Apply the function `f` to `v` if `v` is not an ended computation."
  [f v]
  (if (end? v)
    v
    (map-v f v)))
