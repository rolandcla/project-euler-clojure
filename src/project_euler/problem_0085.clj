(ns project-euler.problem_0085)

;; By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:
;; Although there exists no rectangular grid that contains exactly two million rectangles,
;; find the area of the grid with the nearest solution.

(defn n-of-rect [w h]
  (/ (* w (inc w) h (inc h)) 4))

(defn inv-gauss [x]
  (let [a 1.0
        b 1.0
        c (* x -2)
        d (- (* b b) (* 4 a c))]
    (/ (- (Math/sqrt d) b) 2 a)))

(defn abs [x] (if (< x 0) (- x) x))

(defn solution []
  (->> (for [w (range 1 2000)]
         (let [h (->> (/ 2000000.0 (/ (* w (inc w)) 2))
                      (inv-gauss)
                      (int))]
           [w h]))
       (mapcat (fn [[w h]] [[w h] [w (inc h)]]))
       (map (fn [[w h]] [(abs (- 2000000 (n-of-rect w h))) w h]))
       (sort)
       (first)
       (rest)
       (apply *)
       ))


