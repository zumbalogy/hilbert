(ns hilbert.core
  (require [hilbert.math :as hm :refer [coord->int int->coord]]
           [hilbert.math_d2 :as d2]
           [hilbert.attract :as attract]
           [mikera.image.core :as img]
           [mikera.image.colours :as color]
           [debugger.core :as bug]
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

(defn get-quad [q box]
  (let [h (quot box 2)]
  ))

(defn box [pixels quadrants]
  ; quadrants is to be a series of integers, each going into a sub-quadrant
  ; for example: [0 3] would be in the middle on the right, [0 0 0] top right

  )

(defn break-box [box]
  (let []
    ))

(defn build-box [q1 q2 q3 q4]
  (let [h (count (img/get-pixels q1))
        side (* 2 h)
        box (img/new-image side side)
        pixels (img/get-pixels box)]
    (dotimes [i (* side side)]
      (let [x (rem i side)
            y (quot i side)
            offset-y (quot y 2)]
        (aset pixels i (cond (and (<= x h) (<= y h)) (get (+    x       offset-y)   q1)
                             (and (>  x h) (<= y h)) (get (+ (- x h)    offset-y)   q2)
                             (and (<= x h) (>  y h)) (get (+    x    (- offset-y h) q3))
                             (and (>  x h) (>  y h)) (get (+ (- x h) (- offset-y h) q4))))))
    (img/set-pixels box pixels)
    box)

(defn -main []
  (let [input-pic (img/load-image "resources/fish_300.png")
        pic (img/copy input-pic)
        pic2 (img/sub-image pic 100 100 100 100)
        pixels (img/get-pixels pic)
        width (img/width pic)
        height (img/height pic)
        size (* width height)
        pic7 (build-box pic2 pic2 pic2 pic2)
        ]
    (save pic7 width height)
    ))
