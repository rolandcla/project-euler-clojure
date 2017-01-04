(ns project-euler.problem_0088
  (:require [clojure.math.combinatorics :as combo]))

;; A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers,
;; {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

;; For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

;; For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number.
;; The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

;; k=2: 4 = 2 × 2 = 2 + 2
;; k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
;; k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
;; k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
;; k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

;; Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30;
;; note that 8 is only counted once in the sum.

;; In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.

;; What is the sum of all the minimal product-sum numbers for 2≤k≤12000?

(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))

(defn min-sum-prod [k]
  (->> (range k (+ 1 (* 2 k)))
       (map (fn [n] [n (prime-factors n)]))
       (mapcat (fn [[n pfs]]
                 (->> (combo/partitions pfs)
                      (map (fn [pfs'] [n (map #(apply * %) pfs')]))
                      )))
       (filter (fn [[n pfs]]
                 (let [sum-pfs (apply + pfs)]
                   (== k (+ n (- sum-pfs) (count pfs))))))
       (first)
       (first)
       ))

(defn solution []
  (->> (range 2 13)
       (map min-sum-prod)
       (set)
       (reduce +)))

;;-> 7587457
