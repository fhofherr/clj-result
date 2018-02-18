(ns fhofherr.clj-result)

(defprotocol CljResult
  (error? [this] "Check if the result is an error.")
  (success? [this] "Check if the result is an success.")
  (value [this] "Get the results value."))

(defrecord ^:private CljError [value]
  CljResult
  (error? [_] true)
  (success? [_] false)
  (value [this] (:value this)))

(defrecord ^:private CljSuccess [value]
  CljResult
  (error? [_] false)
  (success? [_] true)
  (value [this] (:value this)))

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
  (if (success? result)
    (-> result value f)
    result))
