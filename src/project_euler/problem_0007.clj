(ns project-euler.problem_0007)

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
;; we can see that the 6th prime is 13.

;; What is the 10 001st prime number?

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

(defn lz-primes
  []
  (concat [2]
          (filter prime? (iterate #(+ 2 %) 3))))


(defn solution []
  (nth (lz-primes) 10000))

;;-> 104743

