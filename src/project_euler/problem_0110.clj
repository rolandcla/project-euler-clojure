(ns project-euler.problem_0110)

;; In the following equation x, y, and n are positive integers.

;; 1   1   1
;; - + - = -
;; x   y   n

;; It can be verified that when n = 1260 there are 113 distinct solutions
;; and this is the least value of n for which the total number of distinct solutions exceeds one hundred.

;; What is the least value of n for which the number of distinct solutions exceeds four million?

;; NOTE: This problem is a much more difficult version of Problem 108
;; and as it is well beyond the limitations of a brute force approach it requires a clever implementation.

(def primes [2, 3, 5, 7, 11,  13, 17, 19, 23, 29,  31, 37, 41, 43, 47,  53, 59, 61, 67, 71, 73, 79, 83, 89])

(defn count-coprime-pairs [ps]
  (->> ps
       (reduce (fn [n k] (+ k n (* 2 n k))))
       ))

(defn pow [x n]
  (loop [p 1 n n]
    (if (zero? n)
      p
      (recur (*' p x) (dec n)))))

(defn apply-factors [ps]
  (->> (map (fn [p k] (pow p k)) primes ps)
       (reduce *')
       ))

(defn count-solutions [ps])

;; En essayant d'optimiser manuellement :
;; (count-coprime-pairs [3 3 2 2 1  1 1 1 1 1  1 1]) -> 4018612

(defn solution []
  (apply-factors [3 3 2 2 1  1 1 1 1 1  1 1]))

;;-> 9350130049860600
