(ns project-euler.problem_0053)

;; There are exactly ten ways of selecting three from five, 12345:

;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

;; In combinatorics, we use the notation, 5C3 = 10.

;; In general,

;;           n!
;; nCr = ----------	
;;        r!(n−r)!

;; ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
;; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

;; How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

(defn fact [x] (reduce * 1N (range 1 (inc x))))

(defn c [n r] (/ (fact n) (* (fact r) (fact (- n r)))))

(defn solution[]
  (->> (for [n (range 1 101), r (range 1 n)] (c n r))
       (filter #(> % 1000000))
       (count)
   ))

;;-> 4075
