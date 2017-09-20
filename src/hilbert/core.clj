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
  (println (int (Math/sqrt (count box))))
  (let [half (quot (count box) 2)
        side (quot (int (Math/sqrt (count box))) 2)
        top (partition side (take half box))
        bottom (partition side (drop half box))
        q1 (int-array (flatten (take-nth 2 top)))
        q2 (int-array (flatten (take-nth 2 (rest top))))
        q3 (int-array (flatten (take-nth 2 bottom)))
        q4 (int-array (flatten (take-nth 2 (rest bottom))))]
    [q1 q2 q3 q4]))

(defn build-box [q1 q2 q3 q4]
  (let [h (count q1)
        side (int (Math/sqrt h))
        box (img/new-image (* 2 side) (* 2 side))
        top (mapcat concat (partition side q1) (partition side q2))
        bottom (mapcat concat (partition side q3) (partition side q4))]
    (int-array (concat top bottom))))

(defn -main []
  (let [input-pic (img/load-image "resources/fish_256.png")
        pic (img/copy input-pic)
        pic2 (img/sub-image pic 100 100 100 100)
        pixels (img/get-pixels pic)
        pixels2 (img/get-pixels pic2)
        [q1 q2 q3 q4] (break-box pixels)
        width (img/width pic)
        height (img/height pic)
        size (* width height)
        box (build-box pixels2 pixels2 pixels2 pixels2)]
    ; (save box 200 200)
    (save q1 128 128)
    (save q2 128 128)
    (save q3 128 128)
    (save q4 128 128)
    ))
