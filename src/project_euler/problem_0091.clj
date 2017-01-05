(ns project-euler.problem_0091)

;; The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin,
;; O(0,0), to form ΔOPQ.

;; There are exactly fourteen triangles containing a right angle that can be formed when
;; each co-ordinate lies between 0 and 2 inclusive;
;; that is, 0 ≤ x1, y1, x2, y2 ≤ 2.

;; Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

(defn contains-right-angle? [x1 y1 x2 y2]
  (zero? (* (+ (* x1 x2) (* y1 y2))
            (+ (* x1 (- x2 x1)) (* y1 (- y2 y1)))
            (+ (* x2 (- x1 x2)) (* y2 (- y1 y2))))))

(defn solution-for [n]
  (->> (for [x1 (range n), y1 (range n), x2 (range n), y2 (range n)] [x1 y1 x2 y2])
       (remove (fn [[x1 y1 x2 y2]] (or (and (== 0 x1) (== 0 y1))
                                       (and (== 0 x2) (== 0 y2))
                                       (and (== x1 x2) (== y1 y2)))))
       (filter (fn [ps] (apply contains-right-angle? ps)))
       (count)
       ((fn [cnt] (/ cnt 2)))
       ))

(defn solution []
  (solution-for 51))

;;-> 14234
