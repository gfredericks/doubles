(ns com.gfredericks.doubles)

;; Reference:
;; https://en.wikipedia.org/w/index.php?title=Double-precision_floating-point_format&oldid=662600606

(def ^:private exp2-1022 (apply * (repeat 1022 2N)))

(defn parse-double
  "Returns [sign exponent fraction] where each is a non-negative
  integer read from the corresponding field of the binary
  representation of the given double."
  [^double x]
  (let [x-long (Double/doubleToRawLongBits x)]
    [(-> x-long (bit-shift-right 63) (bit-and 1))
     (-> x-long (bit-shift-right 52) (bit-and 0x7ff))
     (-> x-long (bit-and 0xfffffffffffff))]))

(defn unparse-double
  [[sign exponent fraction]]
  (Double/longBitsToDouble
   (bit-or (bit-shift-left sign 63)
           (bit-shift-left exponent 52)
           fraction)))

(defn double->data
  [x]
  (let [[sign exp' base'] (parse-double x)
        base (-> base'
                 (/ 0x10000000000000)
                 (+ 1))
        exp (- exp' 1023)
        base-ret {:fields {:sign sign
                           :fraction base'
                           :exponent exp'}}]
    (case exp'
      0 (if (zero? base')
          (assoc base-ret
            :type ::zero
            :value 0
            :signum (case sign 0 1 1 -1))
          (assoc base-ret
            :type ::subnormal
            :value (cond-> (/ base' 0x10000000000000 exp2-1022)
                           (= 1 sign)
                           (-'))))
      2047 (if (zero? base')
             (assoc base-ret
               :type ::infinity
               :signum (case sign 0 1 1 -1))
             (assoc base-ret
               :type ::NaN))
      ;; regular number
      (assoc base-ret
        :type ::normal
        :value (cond-> (* base (if (pos? exp)
                                 (apply * (repeat exp 2N))
                                 (apply * (repeat (- exp) 1/2))))
                       (= 1 sign)
                       (-'))))))

(defn double->exact
  "Returns an exact number (integer or ratio) representing the value
  of the given double, or nil if the double is infinite or NaN.

  Positive and negative zero both return 0."
  [x]
  (-> x double->data :value))

(def ^:const max-pos-double (double->exact Double/MAX_VALUE))
(def ^:const min-pos-double (double->exact Double/MIN_VALUE))
(def ^:const min-neg-double (- max-pos-double))
(def ^:const max-neg-double (- min-pos-double))
(def ^:const ^:private exp-2-52 (apply * (repeat 52 2)))

(let [x ((fn [^Double x] (Math/scalb x -1022)) 1.0)]
  (def ^:const min-normal-pos-double
    (double->exact x))
  (def ^:const max-subnormal-pos-double
    (double->exact (Math/nextAfter ^double x -1.0))))

(def ^:const ^:private subnormal-factor (/ min-pos-double))

(defn ^:private scale
  "Returns [q' k] such that q = q' * 2^k, with 1.0 <= q' < 2."
  [q]
  {:pre [(pos? q)]}
  (if (< q 1)
    (loop [q' q
           k 0]
      (if (<= 1 q')
        [q' k]
        (recur (* q' 2) (dec k))))
    (loop [q' q
           k 0]
      (if (<= 2 q')
        (recur (/ q' 2) (inc k))
        [q' k]))))

(defn exact->double
  "Returns a double if the given integer or ratio is exactly
  representable as a double, else nil."
  [q]
  (cond (zero? q)    0.0
        (neg? q)     (some-> q - exact->double -)
        :else
        (if (<= q max-subnormal-pos-double)
          (let [m (* q subnormal-factor)]
            (when (integer? m)
              ;; this just happens to work, since all the other
              ;; fields can be zero.
              (Double/longBitsToDouble (long m))))
          (let [[q' k] (scale q)]
            (when (<= -1022 k 1023)
              (let [m (* (- q' 1) exp-2-52)]
                (when (integer? m)
                  (Double/longBitsToDouble
                   (bit-or (long m) (bit-shift-left (+ 1023 k) 52))))))))))

(defmethod print-method Double
  [x ^java.io.Writer pw]
  (if (or (Double/isInfinite x)
          (Double/isNaN x))
    (.write pw (str x))
    (let [s (-> x double->exact bigdec str)
          s (subs s 0 (count s))
          s (if (re-find #"\." s) s (str s ".0"))]
      (.write pw s))))

;; E.g.:

;; user> 0.1
;; 0.1000000000000000055511151231257827021181583404541015625
;; user> 0.25
;; 0.25
;; user> 42.0
;; 42.0
;; user> 42.42
;; 42.4200000000000017053025658242404460906982421875
;; user> 1e50
;; 100000000000000007629769841091887003294964970946560.0
