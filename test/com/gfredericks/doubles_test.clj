(ns com.gfredericks.doubles-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.doubles :as doubles]
            [com.gfredericks.test.chuck.generators :as gen']))

(def gen-binary-double
  (gen'/for [:parallel [prob (gen/choose 0 255)
                        bit-sources (gen/vector (gen/choose 0 255) 64)]
             :let [bits (for [x bit-sources]
                          (if (< x prob) 0 1))]]
    (#'doubles/bits->double (vec bits))))

#_
(defspec double->data-round-trip 1000
  (prop/for-all [x (gen/one-of [gen'/double gen-binary-double])]
    (= x (-> x (doubles/double->data) (doubles/data->double)))))

(defn ratio->double
  "Tries to convert a *BINARY* ratio to a double."
  [q]
  (loop [x' (double (numerator q))
         denom (denominator q)]
    (if (= 1 denom)
      x'
      (recur (/ x' 2) (/ denom 2)))))

(defspec double-stuff 1000
  (prop/for-all [^Double x (gen/one-of [gen'/double gen-binary-double])]
    (let [data (doubles/double->data x)]
      (case (:type data)
        ::doubles/zero
        (and (= 0.0 x)
             (= (double (:signum data))
                (Math/copySign 1.0 x)))

        ::doubles/NaN
        (.isNaN x)

        ::doubles/infinity
        (and (.isInfinite x)
             (= x (case (:signum data)
                    1 Double/POSITIVE_INFINITY
                    -1 Double/NEGATIVE_INFINITY)))

        (::doubles/subnormal ::doubles/normal)
        (let [value (:value data)]
          (if (integer? value)
            (= x (double value))
            (= x (ratio->double value))))))))
