(ns project-euler.problem_0082)

;; The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and
;; finishing in any cell in the right column, and only moving up, down, and right,
;; is indicated in red and bold; the sum is equal to 994.

;; Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
;; a 31K text file containing a 80 by 80 matrix, from the left column to the right column.

(def s-matrix [[131 673 234 103  18]
               [201  96 342 965 150]
               [630 803 746 422 111]
               [537 699 497 121 956]
               [805 732 524  37 331]])

;; (defn minimal-path [matrix]
;;   (let [n (count matrix)
;;         m' (-> (vec (map (fn [row] (vec (map (fn [v] [v nil]) row))) matrix))
;;                (update-in [0 0] (fn [[v _]] [v v]))
;;                )]))

(defn minimal-path [matrix]
  (let [n (count matrix)
        m' (->> matrix
                (map (fn [row]
                       (->> row
                            (map-indexed (fn [ix v] [v (if (zero? ix) v nil)]))
                            vec)))
                vec)
        ]
    m'))


(defn h-min-paths-for-col [matrix col]
  (->> matrix
       (map (fn [row]
              (->> row
                   (map-indexed (fn [ix [v min-v]] (if (== col ix)
                                                       [v (+ v (get-in row [(dec ix) 1]))]
                                                       [v min-v])))
                   vec)))
       vec)
  )

(defn v-min-paths-for-col [matrix col-ix]
  (let [col (for [row matrix] (nth row col-ix))]
    col))
