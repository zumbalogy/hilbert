(ns hilbert.core
  (require [hilbert.math :refer [hilbert->int int->hilbert]]
           [clojure.math.combinatorics :as combo]
           [mikera.image.core :as img]
           [mikera.image.colours :as color]))

(defn show [pic]
  (img/show pic :zoom 5 :title (rand)))

(def rgb color/components-rgb)

(defn component [[r g b]]
  (color/rgb-from-components r g b))

(defn make-fish [input]
  (let [fish (img/load-image "resources/fish_300.png")
        pixels (img/get-pixels fish)
        pixels2 (img/get-pixels fish)]
    (dotimes [i (count pixels)]
      (let [p (get pixels i)
            x (mod i 300)
            y (quot i 300)
            h (hilbert->int [x y])
            [x2 y2] (int->hilbert (* h 1) 2)
            h2 (+ x2 (* 300 y2))
            p2 (get pixels (mod h2 (* 300 300)))
            [r g b] (rgb p2)
            h3 (hilbert->int [r g b])
            h4 (* x2 (/ h3 (inc y2)))
            [r2 g2 b2] (int->hilbert h4 3)
            p3 (component [r2 g2 b2])]
        (aset pixels i p3)))
    (img/set-pixels fish pixels)
    (show fish)
    (img/save fish (str "output/" (System/currentTimeMillis) ".png"))))

(defn -main []
  (make-fish 1))
  ; (doseq [i (range 1 1.1 0.002)]
  ;   (println i)
  ;   (make-fish i)))
