(ns hilbert.core
  (require [hilbert.math :as hm :refer [coord->int int->coord]]
           [hilbert.math_d2 :as d2]
           [mikera.image.core :as img]
           [mikera.image.colours :as color]
           [bardo.ease :as ease]))

(defn show [pic]
  (let [width (img/width pic)
        height (img/height pic)
        size (* width height)]
    (img/show pic :zoom (if (< size 100000) 5 1) :title (rand))))

(def rgb color/components-rgb)

(defn component [r g b]
  (color/rgb-from-components r g b))

(defn next-pow2 [x]
  (int (first (filter #(<= x %) (map #(Math/pow 2 %) (range))))))

(defn make-fish [input-pic input]
  (let [pic (img/copy input-pic)
        pixels (img/get-pixels pic)
        pixels2 (img/get-pixels pic)
        width (img/width pic)
        height (img/height pic)
        size (* width height)
        max-h (coord->int [width height])]
    (dotimes [i size]
      (let [p (get pixels i)
            x (rem i width)
            y (quot i width)
            [r g b] (rgb p)
            h (d2/coord->int x y)
            ; h (coord->int [x y])
            ; [x2 y2] (int->coord (rem (+ h input) max-h) 2)
            [x2 y2] (d2/int->coord (rem (+ h input) max-h))
            ; try pushing them by x or y
            h2 (+ x2 (* width y2))
            p2 (get pixels (rem h2 size))]
            ; [r g b] (rgb p)
            ; h3 (coord->int [r g (+ r b x)])
            ; h4 (+ h3 (* b input))
            ; [r2 g2 b2] (int->coord h4 3)
            ; p3 (component r2 g2 b2)]
        (aset pixels2 i p2)))
    (img/set-pixels pic pixels2)
    (show pic)
    (img/save pic (str "output/" (System/currentTimeMillis) ".png")) ; should this  be a future?
    ))

(defn cubic-range [start end n-steps]
  (let [obj (ease/ease :cubic-in-out)
        diff (- end start)
        tiny-range (range 0 1 (/ 1 n-steps))
        obj-range (map obj tiny-range)]
    (map #(+ start (* % diff)) obj-range)))

(defn -main []
  ; (time (coord->int [300 300]))
  ; (println (coord->int [300 300]))
  ; (time (coord->int [300 300]))
  ; (time (coord->int [300 300]))
  ; (time (coord->int [300 300]))
  ; (time (coord->int [301 301]))
  ; (println)
  ; (time (int->coord 123456 2))
  ; (println (int->coord 123456 2))
  ; (time (int->coord 123456 2))
  ; (time (int->coord 123456 2))
  ; (time (int->coord 123456 2))
  ; (time (int->coord 123459 2))
  ; (time (int->coord 123459 3))
  ; )
  ; (let [pic (img/load-image "resources/rainbow_300.png")])
  (let [pic (img/load-image "resources/fish_256.png")
        width (img/width pic)
        height (img/height pic)
        size (* width height)
        h (coord->int [width height])
        start 0
        stop h
        n-steps 100
        step (inc (quot (- stop start) n-steps))
        steps (range start stop step)
        smooth (cubic-range start stop n-steps)]
      (time (make-fish pic 0))))
      ; (time (make-fish pic 300))))
      ; (time (make-fish pic h))))
      ; (doseq [i (range 0 3600 12)]
      ;   (println i " out of " stop)
      ;   (time (make-fish pic i)))))
