(ns project-euler.problem_0067)

;; By starting at the top of the triangle below and moving to adjacent numbers on the row below,
;; the maximum total from top to bottom is 23.

;; 3
;; 7 4
;; 2 4 6
;; 8 5 9 3

;; That is, 3 + 7 + 4 + 9 = 23.

;; Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'),
;; a 15K text file containing a triangle with one-hundred rows.

;; NOTE: This is a much more difficult version of Problem 18.
;; It is not possible to try every route to solve this problem, as there are 2^99 altogether!
;; If you could check one trillion (1012) routes every second
;; it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)

;; From problem 0018
;; ----------------------------------------
(defn max-for-row
  [prev-row-max row]
  (->> (map max
            (concat prev-row-max [0])
            (concat [0] prev-row-max))
       (map + row)
       )
  )
;; ----------------------------------------


(defn solution []
  (->> (slurp "resources/p067_triangle.txt")
       (clojure.string/split-lines)
       (map (fn [line] (map (comp int bigdec) (clojure.string/split line #" "))))
       (reduce max-for-row)
       (apply max)
       ))

;;-> 7273
