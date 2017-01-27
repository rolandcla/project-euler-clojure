(ns project-euler.problem_0124
  (:require [clojure.set :as set]))

;; The radical of n, rad(n), is the product of the distinct prime factors of n.
;; For example, 504 = 23 × 32 × 7, so rad(504) = 2 × 3 × 7 = 42.

;; If we calculate rad(n) for 1 ≤ n ≤ 10, then sort them on rad(n),
;; and sorting on n if the radical values are equal, we get:

;; Unsorted     Sorted
;; n  rad(n)    n  rad(n)  k
;; 1  1         1  1       1
;; 2  2         2  2       2
;; 3  3         4  2       3
;; 4  2         8  2       4
;; 5  5         3  3       5
;; 6  6         9  3       6
;; 7  7         5  5       7
;; 8  2         6  6       8
;; 9  3         7  7       9
;; 10 10        10 10      10

;; Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and E(6) = 9.

;; If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000).


(defn distinct-prime-factors-until [m]
  (loop [n 2
         primes []
         prime-factors {}
         ]
    (if (> n m)
      prime-factors
      (let [[q p] (->> primes
                       (map (fn [p] [(/ n p) p]))
                       (take-while (fn [[q p]] (<= p q)))
                       (filter (fn [[q p]] (integer? q)))
                       (first)
                       )]
        (if p
          (recur (inc n)
                 primes
                 (assoc prime-factors
                        n
                        (set/union (get prime-factors q) #{p})
                        ))
          (recur (inc n)
                 (conj primes n)
                 (assoc prime-factors
                        n
                        #{n}))
          )))))

(defn solution-for [m k]
  (->> (assoc (distinct-prime-factors-until m) 1 #{1})
       (map (fn [[n ps]] [(reduce * ps) n]))
       (sort)
       ((fn [rns] (second (nth rns (dec k)))))

       ))

(defn solution []
  (solution-for 100000 10000))

;;-> 21417
