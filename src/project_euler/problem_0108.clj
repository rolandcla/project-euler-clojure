(ns project-euler.problem_0108
  (:require [clojure.math.combinatorics :as combo]))

;; n the following equation x, y, and n are positive integers.

;; 1   1   1
;; - + - = -
;; x   y   n

;; For n = 4 there are exactly three distinct solutions:

;; 1    1   1
;; - + -- = -
;; 5   20   4

;; 1    1   1
;; - + -- = -
;; 6   12   4

;; 1   1   1
;; - + - = -
;; 8   8   4

;; What is the least value of n for which the number of distinct solutions exceeds one-thousand?

;; NOTE: This problem is an easier version of Problem 110;
;; it is strongly advised that you solve this one first

;;-------------------------------------------------------------------------------------------------

(defn sol-optic-equation [m n k]
  [(* k m (+ m n))
   (* k n (+ m n))
   (* k m n)])

(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))

(defn count-pairs [ns]
  (reduce (fn [n k] (+ k n (* 2 n k))) ns))

(defn solution []
  (->> (iterate #(+ 30 %) 30)
       (map (fn [x]
              [x
               (->> x
                    (prime-factors)
                    (group-by identity)
                    (vals)
                    (map count)
                    )]))
       (drop-while (fn [[x n-pairs]] (<= (count-pairs n-pairs) 1000)))
       (first)
       (first)
       ))

;;-> 180180
