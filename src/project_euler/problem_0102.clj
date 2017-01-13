(ns project-euler.problem_0102)

;; Three distinct points are plotted at random on a Cartesian plane,
;; for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.

;; Consider the following two triangles:

;; A(-340,495), B(-153,-910), C(835,-947)

;; X(-175,41), Y(-421,-714), Z(574,-645)

;; It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.

;; Using triangles.txt (right click and 'Save Link/Target As...'),
;; a 27K text file containing the co-ordinates of one thousand "random" triangles,
;; find the number of triangles for which the interior contains the origin.

;; NOTE: The first two examples in the file represent the triangles in the example given above.

(defn origin-in-triangle? [[x1 y1 x2 y2 x3 y3]]
  (let [d1 (- (* x1 y2) (* x2 y1))
        d2 (- (* x2 y3) (* x3 y2))
        d3 (- (* x3 y1) (* x1 y3))]
    (or (and (> d1 0) (> d2 0) (> d3 0))
        (and (< d1 0) (< d2 0) (< d3 0)))
    ))


(defn solution []
  (->> (slurp "resources/p102_triangles.txt")
       (clojure.string/split-lines)
       ;;(take 5)
       (map (fn [line] (map read-string (clojure.string/split line #","))))
       (filter origin-in-triangle?)
       (count)
       ))

;;-> 228
