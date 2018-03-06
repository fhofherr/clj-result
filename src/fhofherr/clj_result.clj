(ns fhofherr.clj-result
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as cs]))

(defprotocol ^:no-doc CljResult
  (-error? [this] "Check if the result is an error.")
  (-success? [this] "Check if the result is an success.")
  (-value [this] "Get the results value.")
  (-update-value [this f] "Apply the function f to the results value"))

(defn result?
  "Check if `x` is a result."
  [x]
  (satisfies? CljResult x))

(s/def ::clj-result result?)

(defrecord ^:private CljError [value]
  CljResult
  (-error? [_] true)
  (-success? [_] false)
  (-value [this] (:value this))
  (-update-value [this f] (-> value f ->CljError)))

(defrecord ^:private CljSuccess [value]
  CljResult
  (-error? [_] false)
  (-success? [_] true)
  (-value [this] (:value this))
  (-update-value [this f] (-> value f ->CljSuccess)))

(defn error?
  "Check if the result is an error"
  [result]
  (-error? result))

(s/fdef error?
        :args (s/cat :result ::clj-result)
        :ret boolean?)

(defn success?
  "Check if the result is a success"
  [result]
  (-success? result))

(s/fdef success?
        :args (s/cat :result ::clj-result)
        :ret boolean?)

(defn value
  "Get the result value"
  [result]
  (if (result? result)
    (-value result)
    result))

(s/fdef value
        :args (s/cat :result ::clj-result)
        :ret any?)

(defn error
  "Turn `value` into an error."
  [value]
  (->CljError value))

(s/fdef error
        :args (s/cat :value any?)
        :ret (s/and ::clj-result error?))

(defn success
  "Turn `value` into a success."
  [value]
  (cond
    (not (result? value)) (->CljSuccess value)
    (error? value) (throw (IllegalArgumentException.
                            "Can't turn an error into a success"))
    :else value))

(s/fdef success
        :args (s/cat :value any?)
        :ret (s/and ::clj-result success?))

(defn m-bind
  "Apply the function `f` to `v` if `v` is not an error. Do nothing
  to `v` if it is an error.

  `f` is expected to have the type

      v -> w

  where `w` is a potentially updated value `w`."
  [v f]
  {:pre [(fn? f)]
   :post [#(result? %)]}
  (if (and (result? v) (error? v))
    v
    (let [next-result (-> v success value f)]
      (if (result? next-result)
        next-result
        (success next-result)))))

(s/fdef m-bind
        :args (s/cat :result ::clj-result
                     :f (s/fspec :args (s/cat :v any?)
                                 :ret ::clj-result))
        :ret ::clj-result)

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

(s/fdef attempt
        :args (s/cat :bindings ::cs/bindings
                     :body any?))

(defmacro attempt-as->
  "Binds `init-result` to `sym`. If `init-result` is a success it evaluates the
  forms until it exhausted all or one evaluated to an error.  Returns the
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
  (-update-value result f))

(s/fdef map-v
        :args (s/cat :result ::clj-result
                    :f (s/fspec :args (s/cat :x any?)
                                :ret any?))
        :ret ::clj-result)

(defn map-e
  "Apply the function `f` to value wrapped by `result` if `result`
  is an error. Return an error with the updated value."
  [f result]
  (if (error? result)
    (map-v f result)
    result))

(s/fdef map-e
        :args (s/cat :result ::clj-result
                    :f (s/fspec :args (s/cat :x any?)
                                :ret any?))
        :ret ::clj-result)

(defn map-s
  "Apply the function `f` to value wrapped by `result` if `result`
  is a success Return a success with the updated value."
  [f result]
  (if (success? result)
    (map-v f result)
    result))

(s/fdef map-s
        :args (s/cat :result ::clj-result
                    :f (s/fspec :args (s/cat :x any?)
                                :ret any?))
        :ret ::clj-result)
