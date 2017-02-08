(ns hilbert.math_d2)

(defn gray-encode [n]
  (case n
    0 0
    1 1
    2 3
    3 2))
  ; (println bn)
  ; (bit-xor bn (quot bn 2)))

; (defn gray-decode [n]
;   (case n
;     0 0
;     1 1
;     2 3
;     3 2))

(defn gray-decode-travel [start end g]
  (let [travel-bit (bit-xor start end)
        rg (* (bit-xor g start)
              (quot 4 (* 2 travel-bit)))
              ; 3)
              ]
    (case rg
      0 0
      1 1
      2 3
      3 2
      4 1
      6 2)))

(defn gray-encode-travel [start end i]
  (let [travel-bit (bit-xor start end)
        g (* (gray-encode i) (* 2 travel-bit))]
    (bit-xor start (bit-and 3 (bit-or g (quot g 4))))))

(defn child-start-end [parent-start parent-end i]
  ; (= -2 (bit-not 1))
  (let [start-i (max 0 (bit-and (bit-not 1) (dec i)))
        end-i (min 3 (bit-or 1 (inc i)))
        child-start (gray-encode-travel parent-start parent-end start-i)
        child-end (gray-encode-travel parent-start parent-end end-i)]
    [child-start child-end]))

(defn transpose-bits [srcs nDests]
  (let [srcs (atom srcs)
        nSrcs (count @srcs)
        dests (atom (vec (repeat nDests 0)))]
    (doseq [j (range (dec nDests) -1 -1)]
      (let [dest (atom 0)]
        (doseq [k (range nSrcs)]
          (let [sk (get @srcs k)]
            (reset! dest (+ (* 2 @dest) (mod sk 2)))
            (swap! srcs assoc k (quot sk 2))))
        (swap! dests assoc j @dest)))
    @dests))

(def pack-coords transpose-bits)

(defn log [a b]
  (/ (Math/log a) (Math/log b)))

(defn unpack-coords [coords]
  (let [biggest (apply max coords)
        nChunks (-> biggest inc (log 2) Math/ceil int (max 1))]
    (transpose-bits coords nChunks)))

(defn unpack-index [i nD]
  (let [i (atom i)
        p (Math/pow 2 nD)
        nChunks (-> @i inc (log p) Math/ceil int (max 1))
        chunks (atom (vec (repeat nChunks 0)))]
    (doseq [j (range (dec nChunks) -1 -1)]
      (swap! chunks assoc j (int (mod @i p)))
      (swap! i quot p))
    @chunks))

(defn pack-index [chunks nD]
  (let [p (Math/pow 2 nD)]
    (bigint (reduce #(+ %2 (* %1 p)) chunks))))

(defn initial-end [nChunks nD]
  (int (Math/pow 2 (mod (dec (- nChunks)) nD))))

(defn transpose-xy [x y]
  (let [xs (iterate #(quot % 2) x)
        ys (iterate #(quot % 2) y)
        x2s (map #(* 2 (rem % 2)) xs)
        y2s (map #(rem % 2) ys)
        dests (map + x2s y2s)]
    (reverse (take 8 dests)))) ; for coordinates up to 256 (2^8)

(defn coord->int [x y]
  (let [coord-chunks (transpose-xy x y)
        start (atom 0)
        end (atom 2)
        index-chunks (atom [])]
    (doseq [chunk coord-chunks]
      (let [i (gray-decode-travel @start @end chunk)
            [start2 end2] (child-start-end @start @end i)]
        ; (swap! index-chunks conj j) ;
        (swap! index-chunks conj i)
        (reset! start start2)
        (reset! end end2)))
    (pack-index @index-chunks 2)))

(defn unpack-xy-index [i]
  (let [is (iterate #(quot % 4) i)
        chunks (map #(int (mod % (int 4))) is)]
    (vec (reverse (take 8 chunks)))))

(defn int->coord [i]
  (let [index-chunks (unpack-xy-index i)
        start (atom 0)
        end (atom 2)
        coord-chunks (atom [])]
    (doseq [chunk index-chunks]
      (let [[start2 end2] (child-start-end @start @end chunk)]
        (swap! coord-chunks conj (gray-encode-travel @start @end chunk))
        (reset! start start2)
        (reset! end end2)))
    (pack-coords @coord-chunks 2)))
