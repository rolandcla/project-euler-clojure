(ns project-euler.problem_0116)

;; A row of five black square tiles is to have a number of its tiles replaced with
;; coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).

;; If red tiles are chosen there are exactly seven ways this can be done.

;; If green tiles are chosen there are three ways.

;; And if blue tiles are chosen there are two ways.

;; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
;; replacing the black tiles in a row measuring five units in length.

;; How many different ways can the black tiles in a row measuring fifty units in length
;; be replaced if colours cannot be mixed and at least one coloured tile must be used?

;; NOTE: This is related to Problem 117.


(def mz-count-ways
  (memoize
   (fn [row-size tile-size]
     (if (> tile-size row-size)
       0
       (+ 1
          (mz-count-ways (- row-size tile-size) tile-size)
          (mz-count-ways (- row-size 1) tile-size))))))

(defn solution []
  (->> [2 3 4]
       (map #(mz-count-ways 50 %))
       (reduce +)
       ))

;;-> 20492570929

