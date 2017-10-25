(ns hilbert.core
  (require [hilbert.math :as hm :refer [coord->int int->coord]]
           [hilbert.math_d2 :as d2]
           [hilbert.attract :as attract]
           [mikera.image.core :as img]
           [mikera.image.colours :as color]
           [net.n01se.clojure-jna :as jna]
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
        max-h (coord->int [width height])
        max-color-h (coord->int [256 256 256])]
    (dotimes [i size]
      (let [p (get pixels i)
            x (rem i width)
            y (quot i width)
            [r g b] (rgb p)
            h (coord->int [x y])
            ; [x2 y2] (int->coord (rem (+ h input) max-h) 2)
            [x2 y2] (int->coord (rem (+ h input) max-h) 2)
            ; try pushing them by x or y
            h2 (+ x2 (* width y2))
            p2 (get pixels (rem h2 size))]
            ; [r g b] (rgb p)
            ; h3 (coord->int [r g b])
            ; h4 (mod (bigint (+ h3 input)) max-color-h)
            ; [r2 g2 b2] (int->coord h2 3)
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

(defn attract-fish [input-pic _iterations]
  (let [pic (img/copy input-pic)
        pixels (img/get-pixels pic)
        pixels2 (img/get-pixels pic)
        width (img/width pic)
        height (img/height pic)
        size (* width height)]
    (dotimes [i size]
      (let [p (get pixels i)
            x (rem i width)
            y (quot i width)
            [r g b] (rgb p)
            [r2 g2 b2] (attract/aizawa [r x i])
            p3 (component r2 g2 b2)]
        (aset pixels2 i p3)))
    (img/set-pixels pic pixels2)
    (show pic)
    (img/save pic (str "output/" (System/currentTimeMillis) ".png"))
    ))


;; (defn -main []
;;   (dotimes [i 1]
;;    ; (let [pic (img/load-image "resources/fish_256.png")
;;     (let [pic (img/load-image "resources/blue_bottle_1920_1282.jpg")
;;           pixels (img/get-pixels pic)]
;;       (time (jna/invoke Integer curve/test pixels))
;;       (img/set-pixels pic pixels)
;;       (show pic)
;;       (img/save pic (str "output/" (System/currentTimeMillis) ".png"))
;;       )))

(defn -main []
  ; (let [pic (img/load-image "resources/night_800.jpg")
  (let [pic (img/load-image "resources/fish_300.png")
        width (img/width pic)
        height (img/height pic)
        size (* width height)]
      (make-fish pic 1000)))

; (defn -main []
;   ; (let [pic (img/load-image "resources/night_800.jpg")
;   (let [pic (img/load-image "resources/fish_256.png")
;         width (img/width pic)
;         height (img/height pic)
;         size (* width height)
;         max-color-h (coord->int [256 256 256])
;         h (coord->int [width height])
;         start 0
;         stop max-color-h
;         n-steps 128
;         step (inc (quot (- stop start) n-steps))
;         steps (range start stop step)
;         smooth (cubic-range start stop n-steps)]
;       (make-fish pic 0)))
;       ; (time (make-fish pic 0))
;       ; (time (make-fish pic max-color-h))))
;       ; (time (make-fish pic h))))
;       ; (doseq [i (range 0 h 12)]
;       ;   (println i " out of " stop)
;         ; (doseq [i smooth]
;         ;   (make-fish pic (int i))
;         ;   (println (int i) " out of " stop))))
