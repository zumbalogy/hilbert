(ns hilbert.attract)

(def a (atom 0.003))
(def b (atom 28))
(def c (atom 10))
(def d (atom 2.6666))


(defn clifford [[x y]]
  (let [x2 (+ (Math/sin (* @a y)) (* @c (Math/cos (* @a x))))
        y2 (+ (Math/sin (* @b x)) (* @d (Math/cos (* @b y))))]
    [x2 y2]))


(defn lorenz [[x y z]]
  (let [x2 (+ x (* 0.01 @b (- y x)))
        y2 (+ y (* 0.01 (- (* x (- 28 z)) y)))
        z2 (+ z (* 0.01 (- (* y x) (* @d z))))]
    [x2 y2 z2]))
