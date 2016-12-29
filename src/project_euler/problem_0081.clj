(ns project-euler.problem-0081)

;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
;; by only moving to the right and down, is indicated in bold red and is equal to 2427.

;; Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
;; a 31K text file containing a 80 by 80 matrix,
;; from the top left to the bottom right by only moving right and down.

(def s-matrix [[131 673 234 103  18]
               [201  96 342 965 150]
               [630 803 746 422 111]
               [537 699 497 121 956]
               [805 732 524  37 331]])

(defn minimal-path [matrix]
  (let [n (count matrix)
        m' (-> (vec (map (fn [row] (vec (map (fn [v] [v nil]) row))) matrix))
               (update-in [0 0] (fn [[v _]] [v v]))
               )]
    (->> (for [x (range 1 n) y (range (inc x))] [x y])
         (reduce
          (fn [m [x y]]
            (-> m
                (update-in [y x] (fn [[v _]] (if (zero? y)
                                               [v (+ v (get-in m [y (dec x) 1]))]
                                               [v (min (+ v (get-in m [y (dec x) 1]))
                                                       (+ v (get-in m [(dec y) x 1])))])))
                (update-in [x y] (fn [[v _]] (if (zero? y)
                                               [v (+ v (get-in m [(dec x) y 1]))]
                                               [v (min (+ v (get-in m [(dec x) y 1]))
                                                       (+ v (get-in m [x (dec y) 1])))])))
                ))
          m')
         )))

(defn solution []
  (->> (slurp "resources/p081_matrix.txt")
       (clojure.string/split-lines)
       (map (fn [line] (vec (map (comp int bigdec) (clojure.string/split line #",")))))
       (vec)
       (minimal-path)
       (last)
       (last)
       (second)
       ))

;;-> 427337

