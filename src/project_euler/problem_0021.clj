(ns project-euler.problem_0021)

;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;; If d(a) = b and d(b) = a, where a ≠ b,
;; then a and b are an amicable pair and each of a and b are called amicable numbers.

;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
;; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

;; Evaluate the sum of all the amicable numbers under 10000.

;; from problem 0012 ....................................................................................
(defn factors
  [x]
  (->> (range 1 (inc (int (Math/sqrt x))))
       (filter #(== 0 (rem x %)))
       (mapcat #(let [q (/ x %)]
                  (if (== q %) [%] [% q])))
       ))
;;........................................................................................................

(defn sum-of-proper-divisors
  [n]
  (->> (factors n)
       (filter #(not (== n %)))
       (reduce +)
       ))

(defn amicable?
  [n]
  (let [s (sum-of-proper-divisors n)]
    (and (not (== n s))
         (== n (sum-of-proper-divisors s)))
    )
  )

(defn solution []
  (->> (range 1 10000)
       (filter amicable?)
       (set)
       (reduce +)
       ))

;;-> 31626


