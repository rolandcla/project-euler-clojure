(ns project-euler.problem_0147)

;; In a 3x2 cross-hatched grid, a total of 37 different rectangles could be situated within that grid
;; as indicated in the sketch.

;; There are 5 grids smaller than 3x2, vertical and horizontal dimensions being important,
;; i.e. 1x1, 2x1, 3x1, 1x2 and 2x2.
;; If each of them is cross-hatched,
;; the following number of different rectangles could be situated within those smaller grids:

;; 1x1: 1
;; 2x1: 4
;; 3x1: 8
;; 1x2: 4
;; 2x2: 18

;; Adding those to the 37 of the 3x2 grid, a total of 72 different rectangles could be situated
;; within 3x2 and smaller grids.

;; How many different rectangles could be situated within 47x43 and smaller grids?

(defn count-sub-segments [l]
  (/ (* (inc l) l) 2))

(defn count-hv-rect [w h]
  (* (count-sub-segments w) (count-sub-segments h)))

(defn count-diag-rect [w h]
  (let [[w h] (if (< w h) [h w] [w h])
        h2 (+ h h)]
    (->> (range 1 h2)
         (mapcat (fn [h-diag]
                   (when (> w 1)
                     (let [d (- h2 h-diag)]
                       (conj
                        (->> (range 2 d 2)
                             (map (fn [w-diag] [h-diag w-diag 2])))
                        [h-diag d (if (even? d) (- w h -1) (- w h))]
                        )))))
         (map (fn [[_ l n]] (* n (count-sub-segments l))))
         (apply +))))

;; 8x5
;; h-diag
;;   1      [2 4 6 8]*2  [9]*3
;;   2      [2 4 6]*2    [8]*4
;;   3      [2 4 6]*2    [7]*3
;;   4      [2 4]*2      [6]*4
;;   5      [2 4]*2      [5]*3
;;   6      [2]*2        [4]*4
;;   7      [2]*2        [3]*3
;;   8                   [2]*4
;;   9                   [1]*3

;; 8x6
;; h-diag
;;   1     [2 4 6 8 10]*2 [11]*2
;;   2     [2 4 6 8]*2    [10]*3
;;   3     [2 4 5 8]*2    [9]*2
;;...

;; 6x6
;; h-diag
;;   1    [2 4 6 8 10]*2
;;   2    [2 4 6 8]*2    [10]*1
;;   3    [2 4 6 8]*2
;;   4    [2 4 6]*2      [8]*1

;; 3x2
;; h-diag
;;   1    [2]*2          [3]*1
;;   2                   [2]*2
;;   3                   [1]*1

(defn count-rect [w h]
  (+ (count-hv-rect w h)
     (count-diag-rect w h)))

(defn solution-for [w h]
  (->> (for [ww (range 1 (inc w))
             hh (range 1 (inc h))]
         (count-rect ww hh))
       (apply +)))

(defn solution []
  (solution-for 47 43))

;; 846910284



