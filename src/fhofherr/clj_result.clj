(ns fhofherr.clj-result
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as cs]))

(defprotocol ^:no-doc CljResult
  (-value [this] "Get the results value.")
  (-update-value [this f] "Apply the function f to the results value"))

(defn result?
  "Check if `x` is a result."
  [x]
  (satisfies? CljResult x))

(s/def ::clj-result result?)

(defrecord ^:private CljError [value]
  CljResult
  (-value [this] (:value this))
  (-update-value [this f] (-> value f ->CljError)))

(defn value
  "Get the result value"
  [result]
  (if (result? result)
    (-value result)
    result))

(defn error
  "Turn `value` into an error."
  [value]
  (->CljError value))

(defn m-bind
  "Apply the function `f` to `v` if `v` is not an error. Do nothing
  to `v` if it is an error.

  `f` is expected to have the type

      v -> w

  where `w` is a potentially updated value `w`."
  [v f]
  {:pre [(fn? f)]
   :post [#(result? %)]}
  (if (result? v)
    v
    (-> v value f)))

(defn- swap-pairs
  [xs]
  {:pre [(even? (count xs))]}
  (let [xf (comp (partition-all 2)
                 (mapcat (fn [[x y]] [y x])))]
    (sequence xf xs)))

(defn- emit-m-bind
  [[r s & xs] body]
  (if xs
    `(m-bind ~r (fn [~s] ~(emit-m-bind xs body)))
    `(m-bind ~r (fn [~s] ~@body))))

(defmacro attempt
  "Attempt all given operations and bind the values to the respective symbols.
  Return the result of evaluating `body`. Abort immediately if one operation
  returns an error and return the error instead of the body."
  [bindings & body]
  (let [swapped (swap-pairs bindings)]
    (emit-m-bind swapped body)))

(s/fdef attempt
        :args (s/cat :bindings ::cs/bindings
                     :body any?))

(defmacro attempt-as->
  "Binds `init-result` to `sym`. If `init-result` is not an error it evaluates
  the forms until it exhausted all or one evaluated to an error. Returns the
  result of the last form, or the first encountered error."
  [init-result sym & forms]
  `(attempt [~sym ~init-result ~@(mapcat #(vector sym %) forms)] ~sym))

(s/fdef attempt-as->
        :args (s/cat :init-result any?
                     :sym ::cs/local-name
                     :forms (s/* any?)))

(defn map-v
  "Apply the function `f` to the value wrapped by `result`. Return a result
  of the same kind."
  [f result]
  (if (result? result)
    (-update-value result f)
    (f result)))

(defn map-e
  "Apply the function `f` to value wrapped by `result` if `result`
  is an error. Return an error with the updated value."
  [f result]
  (if (result? result)
    (map-v f result)
    result))

(defn map-s
  "Apply the function `f` to `v` if `v` is not an error."
  [f result]
  (if ((complement result?) result)
    (map-v f result)
    result))
