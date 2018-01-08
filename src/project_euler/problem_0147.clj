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
                     (concat
                      (->> (range 2 (inc (- h2 h-diag)))
                           (map (fn [w-diag] [[h-diag w-diag] [h-diag w-diag]]))))))))))
