(ns project-euler.problem_0048)


;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(defn exp-mod [x n]
  (loop [n n, p 1]
    (if (zero? n)
      p
      (recur (dec n) (rem (* p x) 10000000000)))
    )
  )

(defn add-mod [a b]
  (rem (+ a b) 10000000000))

(defn solution []
  (->> (range 1 1001)
         (map (fn [x] (exp-mod x x)))
         (reduce add-mod)
         ))

(solution)
;; 9110846700
