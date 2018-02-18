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
