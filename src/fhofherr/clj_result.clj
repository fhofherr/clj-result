(ns fhofherr.clj-result)

(defprotocol ^:no-doc CljResult
  (-error? [this] "Check if the result is an error.")
  (-success? [this] "Check if the result is an success.")
  (-value [this] "Get the results value."))

(defrecord ^:private CljError [value]
  CljResult
  (-error? [_] true)
  (-success? [_] false)
  (-value [this] (:value this)))

(defrecord ^:private CljSuccess [value]
  CljResult
  (-error? [_] false)
  (-success? [_] true)
  (-value [this] (:value this)))

(defn error?
  "Check if the result is an error"
  [result]
  (-error? result))

(defn success?
  "Check if the result is a success"
  [result]
  (-success? result))

(defn value
  "Get the result value"
  [result]
  (-value result))

(defn error
  "Turn `value` into an error."
  [value]
  (->CljError value))

(defn success
  "Turn `value` into a success."
  [value]
  (->CljSuccess value))

(defn m-bind
  "Apply the function `f` to `result` if `result` is a success. Do nothing
  to `result` if it is an error.

  `f` is expected to have the type:

      v -> R w

  where `v` is the value of the result passed to `m-bind`, `R` is a new result
  with a potentially updated value `w`."
  [result f]
  (if (-success? result)
    (-> result value f)
    result))

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
    `(m-bind ~r (fn [~s] (success ~@body)))))

(defmacro attempt
  "Attempt all given operations and bind the values to the respective symbols.
  Return the result of evaluating `body` as a `success`. Abort immediately if
  one operations returns an error and return the error instead of the body."
  [bindings & body]
  (let [swapped (swap-pairs bindings)]
    (emit-m-bind swapped body)))

(defmacro attempt-v
  "Attempt all given operations just like [[attempt]] but return the value
  of the success or error."
  [bindings & body]
  `(value (attempt ~bindings ~@body)))
