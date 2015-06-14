(ns com.gfredericks.doubles)

;; Reference:
;; https://en.wikipedia.org/w/index.php?title=Double-precision_floating-point_format&oldid=662600606

(defn ^:private byte->bits
  [x]
  (mapv (fn [i]
          (-> x (bit-shift-right i) (bit-and 1)))
        (range 7 -1 -1)))

(defn ^:private double->bits
  "Returns a 64-count vector of 0/1."
  [^double x]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (doto (java.io.DataOutputStream. baos)
      (.writeDouble x))
    (vec (mapcat byte->bits (.toByteArray baos)))))

(defn ^:private bits->byte
  [bits]
  (let [x (->> (range 1 8)
               (map (fn [i]
                      (bit-shift-left (bits i)
                                      (- 7 i))))
               (reduce bit-or))]
    (cond-> x (= 1 (bits 0)) (+ -128))))

(defn ^:private bits->double
  "Input should be a 64-length vector of 0/1."
  [bits]
  (let [bytes (->> bits
                   (partition 8)
                   (map vec)
                   (map bits->byte)
                   (into-array Byte/TYPE))]
    (.readDouble (java.io.DataInputStream. (java.io.ByteArrayInputStream. bytes)))))

(defn ^:private bits->integer
  "High bits first, no negatives. Returns a long."
  [bits]
  (let [bit-count (count bits)]
    (assert (<= bit-count 63))
    (reduce bit-or (map-indexed (fn [i bit]
                                  (bit-shift-left bit (- bit-count i 1)))
                                bits))))

(def exp2-1022 (apply * (repeat 1022 2N)))

(defn double->data
  [x]
  (let [bits (double->bits x)
        sign (bits 0)
        exp-bits (subvec bits 1 12)
        fraction-bits (subvec bits 12 64)
        base' (bits->integer fraction-bits)
        base (-> base'
                 (/ 0x10000000000000)
                 (+ 1))
        exp' (bits->integer exp-bits)
        exp (- exp' 1023)
        base-ret {:fields {:sign sign
                           :base base'
                           :exp exp'}}]
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
