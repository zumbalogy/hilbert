(ns hilbert.math)

; (set! *unchecked-math* true)

(defn gray-encode [bn]
  (bit-xor bn (quot bn 2)))

(defn gray-decode [n]
  (let [sh (atom 1)
        dv (atom (bit-shift-right n @sh))
        n2 (atom (bit-xor n @dv))]
    (while (> @dv 1)
      (reset! sh (bit-shift-left @sh 1))
      (reset! dv (bit-shift-right @n2 @sh))
      (reset! n2 (bit-xor @n2 @dv)))
    @n2))

(defn gray-decode-travel [start end mask g]
  (let [travel-bit (bit-xor start end)
        modulus (inc mask)
        rg (* (bit-xor g start)
              (quot modulus (* 2 travel-bit)))]
    (gray-decode (bit-and mask (bit-or rg (quot rg modulus))))))

(defn gray-encode-travel [start end mask i]
  (let [travel-bit (bit-xor start end)
        modulus (inc mask)
        g (* (gray-encode i) (* 2 travel-bit))]
    (bit-xor start (bit-and mask (bit-or g (quot g modulus))))))

(defn child-start-end [parent-start parent-end mask i]
  ; (= -2 (bit-not 1))
  (let [start-i (max 0 (bit-and (bit-not 1) (dec i)))
        end-i (min mask (bit-or 1 (inc i)))
        child-start (gray-encode-travel parent-start parent-end mask start-i)
        child-end (gray-encode-travel parent-start parent-end mask end-i)]
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

(defn int->coord [i nD]
  (let [index-chunks (unpack-index i nD)
        nChunks (count index-chunks)
        mask (dec (int (Math/pow 2 nD)))
        start (atom 0)
        end (atom (initial-end nChunks nD))
        coord-chunks (atom (vec (repeat nChunks 0)))]
    (doseq [j (range nChunks)]
      (let [i (get index-chunks j)
            [start2 end2] (child-start-end @start @end mask i)]
        (swap! coord-chunks assoc j (gray-encode-travel @start @end mask i))
        (reset! start start2)
        (reset! end end2)))
    (pack-coords @coord-chunks nD)))

(defn coord->int [coords]
  (let [nD (count coords)
        coord-chunks (unpack-coords coords)
        nChunks (count coord-chunks)
        mask (dec (int (Math/pow 2 nD)))
        start (atom 0)
        end (atom (initial-end nChunks nD))
        index-chunks (atom (vec (repeat nChunks 0)))]
    (doseq [j (range nChunks)]
      (let [i (gray-decode-travel @start @end mask (get coord-chunks j))
            [start2 end2] (child-start-end @start @end mask i)]
        (swap! index-chunks assoc j i)
        (reset! start start2)
        (reset! end end2)))
    (pack-index @index-chunks nD)))

(defn transpose-xy [x y]
  (let [xs (iterate #(quot % 2) x)
        ys (iterate #(quot % 2) y)
        x2s (map #(* 2 (rem % 2)) xs)
        y2s (map #(rem % 2) ys)
        dests (map + x2s y2s)]
    (reverse (take 8 dests)))) ; for coordinates up to 256 (2^8)

(defn xy->int [x y]
  (let [coord-chunks (transpose-xy x y)
        start (atom 0)
        end (atom 2)
        index-chunks (atom [])]
    (doseq [chunk coord-chunks]
      (let [i (gray-decode-travel @start @end 3 chunk)
            [start2 end2] (child-start-end @start @end 3 i)]
        ; (swap! index-chunks conj j) ;
        (swap! index-chunks conj i)
        (reset! start start2)
        (reset! end end2)))
    (pack-index @index-chunks 2)))

(defn unpack-index [i nD]
  (let [i (atom i)
        p (Math/pow 2 nD)
        nChunks (-> @i inc (log p) Math/ceil int (max 1))
        chunks (atom (vec (repeat nChunks 0)))]
    (doseq [j (range (dec nChunks) -1 -1)]
      (swap! chunks assoc j (int (mod @i p)))
      (swap! i quot p))
    @chunks))

(defn unpack-index2 [i]
  (let [is (iterate #(quot % 4) i)
        chunks (map #(int (mod % (int 4))) is)]
    (vec (reverse (take 8 chunks)))))

(defn int->xy [i]
  (let [index-chunks (unpack-index2 i)
        nChunks (count index-chunks)
        start (atom 0)
        end (atom (initial-end nChunks 2))
        coord-chunks (atom (vec (repeat nChunks 0)))]
    (doseq [j (range nChunks)]
      (let [i (get index-chunks j)
            [start2 end2] (child-start-end @start @end 3 i)]
        (swap! coord-chunks assoc j (gray-encode-travel @start @end 3 i))
        (reset! start start2)
        (reset! end end2)))
    (pack-coords @coord-chunks 2)))
