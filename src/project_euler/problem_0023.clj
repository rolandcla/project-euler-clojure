(ns project-euler.problem_0023)

;; A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
;; For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
;; which means that 28 is a perfect number.

;; A number n is called deficient if the sum of its proper divisors is less than n
;; and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
;; the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis,
;; it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
;; However, this upper limit cannot be reduced any further by analysis even though it is known that
;; the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

;; Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

;; from problem 0012 ....................................................................................
(defn factors
  [x]
  (->> (range 1 (inc (int (Math/sqrt x))))
       (filter #(== 0 (rem x %)))
       (mapcat #(let [q (/ x %)]
                  (if (== q %) [%] [% q])))
       ))
;;........................................................................................................

;; from problem 0021 ....................................................................................
(defn sum-of-proper-divisors
  [n]
  (->> (factors n)
       (filter #(not (== n %)))
       (reduce +)
       ))
;;........................................................................................................

(require '[clojure.set])

(defn adundant?
  [n]
  (< n (sum-of-proper-divisors n))
  )

(defn solution []
  (let [adundant-numbers
        (filter adundant? (range 1 28123))]
    (->> (for [a adundant-numbers b adundant-numbers] (+ a b))
         (filter #(< % 28123))
         (set)
         (clojure.set/difference (set (range 1 28123)))
         (reduce +)
         )))

(solution)

;;-> 4179871








