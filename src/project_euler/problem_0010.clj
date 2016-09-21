(ns project-euler.problem_0010)

;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;; Find the sum of all the primes below two million.

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
  (reduce + (take-while #(< % 2000000) (lz-primes))))

;;-> 142913828922



