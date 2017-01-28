(ns project-euler.problem-0126)

;; The minimum number of cubes to cover every visible face on a cuboid measuring 3 x 2 x 1 is twenty-two.

;; If we then add a second layer to this solid it would require forty-six cubes to cover every visible face,
;; the third layer would require seventy-eight cubes,
;; and the fourth layer would require one-hundred and eighteen cubes to cover every visible face.

;; However, the first layer on a cuboid measuring 5 x 1 x 1 also requires twenty-two cubes;
;; similarly the first layer on cuboids measuring 5 x 3 x 1, 7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.

;; We shall define C(n) to represent the number of cuboids that contain n cubes in one of its layers.
;; So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.

;; It turns out that 154 is the least value of n for which C(n) = 10.

;; Find the least value of n for which C(n) = 1000.



(defn surface [x y z lev]
  (+ (* 2 z (+ x y))
     (* 4 (- lev 1) (+ x y z lev -2))
     (* 2 x y)))

(defn solution-for-smax [smax cnt]
  (->> (for [x (range 1 (/ smax 4))
             y (range x (/ smax x 2))
             z (range y (/ (- smax (* 2 x y)) (* 2 (+ x y))))
             ]
         [x y z])
       (mapcat (fn [[x y z]]
                 (->> (iterate inc 1)
                      (map (fn [l] (surface x y z l)))
                      (take-while #(< % smax))
                      )))
       (reduce (fn [cnts s]
                 (update cnts s (fn [c] (if c (inc c) 1)))
                 ;;(assoc cnts s (inc (get cnts s 0)))
                 )
               {})
       (filter (fn [[x x-cnt]] (== cnt x-cnt)))
       (sort)
       (first)
       (first)
       ))

(defn solution []
  (solution-for-smax 20000 1000))

;;-> 18522
