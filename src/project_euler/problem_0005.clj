(ns project-euler.problem_0005)

;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10
;; without any remainder.

;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn divisible-by-range?
 [r n]
  (every? #(== 0 (rem n %)) r))

(defn divisible-by-1-to-20? [n] (divisible-by-range? (range 1 21) n))

(defn solution []
  (first (filter divisible-by-1-to-20?
                 (iterate #(+ 2520 %) 2520) )))

;;-> 232792560

