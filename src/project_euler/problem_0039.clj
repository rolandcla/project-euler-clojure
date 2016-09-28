(ns project-euler.problem_0039)

;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
;; there are exactly three solutions for p = 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p ≤ 1000, is the number of solutions maximised?

(def max-side-a (+ 1 (int (/ 1000 (+ 2 (Math/sqrt 2))))))

(defn solution []
  (->> (for [a (range 1 max-side-a)
             b (range a (/ (- 1000 a) 2))]
         [a b (int (+ 0.5 (Math/sqrt (+ (* a a) (* b b)))))])
       (filter (fn [[a b c]] (== (+ (* a a) (* b b)) (* c c))))
       (map #(apply + %))
       (reduce (fn [ms s]
                 (update ms s #(if (nil? %) 1 (inc %)))
                 ) {})
       (seq)
       (map (fn [[s cnt]] [cnt s]))
       (sort)
       (last)
       (second)
       ))

(solution)

;;-> 840

