(ns project-euler.problem-0071)

;; Consider the fraction, n/d, where n and d are positive integers.
;; If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.

;; By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size,
;; find the numerator of the fraction immediately to the left of 3/7.

(defn pgcd [a b]
  (let [r (rem a b)]
    (if (zero? r)
      b
      (recur b r))))

(defn brute-force-solution []
  (->> (range 8 1000001)
       (map (fn [d] [(quot (* 3 d) 7) d]))
       (filter (fn [[n d]] (== 1 (pgcd n d))))
       (map (fn [[n d]] [n d (- 3/7 (/ n d))]))
       (reduce (fn [[n-min d-min diff-min :as fr-min] [n d diff :as fr]]
                 (if (< diff diff-min) fr fr-min))
               [nil nil 1])
       (first)
       ))

(def solution brute-force-solution)
;;-> 428570
