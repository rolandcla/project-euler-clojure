(ns project-euler.problem_0117)

;; Using a combination of black square tiles and oblong tiles chosen from:
;; red tiles measuring two units,
;; green tiles measuring three units,
;; and blue tiles measuring four units,
;; it is possible to tile a row measuring five units in length in exactly fifteen different ways.

;; How many ways can a row measuring fifty units in length be tiled?

;; NOTE: This is related to Problem 116

(def mz-count-ways
  (memoize
   (fn [row-size tile-sizes]
     (let [tss
           (->> tile-sizes
                (remove (fn [tile-size] (> tile-size row-size)))
                )]
       (if (seq tss)
         (reduce (fn [n ts]
                   (+ n 1 (mz-count-ways (- row-size ts) tile-sizes)))
                 (mz-count-ways (- row-size 1) tile-sizes)
                 tss)
         0
         )
       ))))

(defn solution []
  (+ 1 (mz-count-ways 50 [2 3 4]))
  )

;;-> 100808458960497

