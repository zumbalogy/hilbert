(ns hilbert.core
  (require [hilbert.math :as hm :refer [coord->int int->coord]]
           [hilbert.math_d2 :as d2]
           [hilbert.attract :as attract]
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

(defn save
  ([pixels]
    (let [sq (Math/floor (Math/sqrt pixels))]
      (save sq sq)))
  ([pixels w h]
    (let [pic (img/new-image w h)]
      (img/set-pixels pic pixels)
      (show pic)
      (img/save pic (str "output/" (System/currentTimeMillis) ".png")))))

(defn -main []
  (let [input-pic (img/load-image "resources/fish_300.png")
        pic (img/copy input-pic)
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
            gray (quot (* r g) 228)
            p2 (component gray gray gray)
            ; p3 (get pixels (* size (rand)))
            ]
        (aset pixels2 i p2)))
    (save pixels2 width height)))
