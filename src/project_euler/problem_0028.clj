(ns project-euler.problem_0028)

;; Starting with the number 1 and moving to the right in a clockwise direction
;; a 5 by 5 spiral is formed as follows:

;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13

;; It can be verified that the sum of the numbers on the diagonals is 101.

;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

(defn beg-of-spirals
 [n i s]
 (lazy-seq
  (if (> i n)
    nil
    (let [new-s (+ s (* 4 (- i 1)))]
      (cons [i  s] (beg-of-spirals n (+ i 2) new-s))
      ))))

(defn sum-of-diagonals
  [n]
  (->> (beg-of-spirals n 3 2)
       (map (fn [[sq-len beg]] (+ -4 (* 4 beg) (* 10 (dec sq-len)))))
       (reduce +)
       (inc)
       )
  )

(defn solution []
  (sum-of-diagonals 1001))

;;-> 669171001

