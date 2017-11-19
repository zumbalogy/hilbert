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

(defn duffing [[x y z]]
  (let [h (/ @a 20)
        x2 (+ x (* h y))
        y2 (+ y (* h (+ (- x (* x x x) (* @c y)) (* @d (Math/cos z)))))
        z2 (+ z h)]
    [x2 y2 z2]))

(defn aizawa [[x y z]]
  (let [t 0.01
        ; e 0.25
        e (/ @c 2)
        a 0.95
        l 0.6
        ; d 3.5
        d (* 3.7 @d)
        b 0.7
        c 0.1
        x2 (+ x (* t (- (* (- z b) x) (* d y))))
        y2 (+ y (* t (+ (* d x) (* y (- z b)))))
        czxxx (* c z x x x)
        ez (+ 1 (* e z))
        xxyy (+ (* x x) (* y y))
        z3 (/ (* z z z) 3)
        az (* a z)
        z2 (+ z (* t (- (+ l az czxxx) z3 (* ez xxyy))))]
    [x2 y2 z2]))

;; (defn dejong [[x y]]
;;   ; all constants have to be between pi and negative pi
;;   (let [x2 (- (Math/sin (* @a y) (Math/cos (* @b x))))
;;         y2 (- (Math/sin (* (+ @c 1) x)) (Math/cos (* (+ @d 1) y)))]
;;     [x2 y2]))

;; (defn svensson [[x y]]
;;   (let [x2 (- (* @d (Math/sin (* @a x))) (Math/sin (* @b y)))
;;         y2 (- (* @c (Math/cos (* @a x))) (Math/cos (* @b y)))]
;;     [x2 y2]))


;; (defn duffing [[x y]]
;;   (let [h (/ @a 20)
;;         x2 (+ x (* h y))
;;         y2 (+ y (* h (+ (- x (* x x x) (* @c y)) (* @d (Math/cos @z)))))
;;         z2 (+ @z h)]
;;     (reset! z z2)
;;     [x2 y2]))

(defn triz [[x y]]
  (let [x2 (/ x 2)
        y2 (/ y 2)]
    (rand-nth [[x2 y2]
              ;  [(+ x2 0.5) (+ y2 0.86)]
               [(+ x2 1) y2]
               [x2 (- y2 1)]
               [x2 (+ y2 1)]
               [(- x2 1) y2]])))
