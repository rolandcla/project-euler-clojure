(ns project-euler.problem_0093
  (:require [clojure.math.combinatorics :as combo]))

;; By using each of the digits from the set, {1, 2, 3, 4}, exactly once,
;; and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses,
;; it is possible to form different positive integer targets.

;; For example,

;; 8 = (4 * (1 + 3)) / 2
;; 14 = 4 * (3 + 1 / 2)
;; 19 = 4 * (2 + 3) − 1
;; 36 = 3 * 4 * (2 + 1)

;; Note that concatenations of the digits, like 12 + 34, are not allowed.

;; Using the set, {1, 2, 3, 4},
;; it is possible to obtain thirty-one different target numbers of which 36 is the maximum,
;; and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.

;; Find the set of four distinct digits, a < b < c < d,
;; for which the longest set of consecutive positive integers, 1 to n,
;; can be obtained, giving your answer as a string: abcd.


(def exp-trees
  [
   (fn [[op1 op2 op3] [a b c d]]
     (try 
       (op1 (op2 a b) (op3 c d))
       (catch ArithmeticException e nil))
     )

   (fn [[op1 op2 op3] [a b c d]]
     (try 
       (op1 (op2 (op3  a b) c) d)
       (catch ArithmeticException e nil))
     )

   ;; (fn [[op1 op2 op3] [a b c d]]
   ;;   (try 
   ;;     (op1 a (op2 b (op3 c d)))
   ;;     (catch ArithmeticException e nil))
   ;;   )
   ])

(defn target-numbers [ds]
  (->> (combo/combinations [+ + + - - - * * * / / /] 3)
       (mapcat combo/permutations)
       (combo/cartesian-product (combo/permutations ds))
       (mapcat (fn [[xs ops]] (map (fn [ex] (ex ops xs)) exp-trees)))
       (filter (fn [y] (and (integer? y) (> y 0))))
       (set)
       ))

(defn n-of-consecutive [xs]
  (->> (drop-while (fn [x] (xs x)) (iterate inc 1))
       (first)))

(defn solution []
  (->> (combo/combinations (range 1 10) 4)
       (map (fn [ds] [ds (n-of-consecutive (target-numbers ds))]))
       (reduce (fn [ds-l-max ds-l] (if (> (second ds-l) (second ds-l-max))
                                     ds-l
                                     ds-l-max))
               [nil 0])
       (first)
       (apply str)
       ))

;;-> 1258
