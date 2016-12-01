(ns hilbert.core
  (require [hilbert.math :refer [hilbert->int int->hilbert]]))



(defn -main []
  (println (hilbert->int [1 2 90])))
; 864901
