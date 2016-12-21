(ns project-euler.problem_0061
  (:require [clojure.math.combinatorics :as combo]))

;; Triangle, square, pentagonal, hexagonal, heptagonal,
;; and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:

;; Triangle	 	  P3,n=n(n+1)/2	 	  1, 3, 6, 10, 15, ...
;; Square	 	    P4,n=n2	 	        1, 4, 9, 16, 25, ...
;; Pentagonal	 	P5,n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
;; Hexagonal	 	P6,n=n(2n−1)	 	  1, 6, 15, 28, 45, ...
;; Heptagonal	 	P7,n=n(5n−3)/2	 	1, 7, 18, 34, 55, ...
;; Octagonal	 	P8,n=n(3n−2)	 	  1, 8, 21, 40, 65, ...
;; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.

;; The set is cyclic, in that the last two digits of each number is the first two digits of the next number
;; (including the last number with the first).
;; Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882),
;; is represented by a different number in the set.
;; This is the only set of 4-digit numbers with this property.
;; Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type:
;; triangle, square, pentagonal, hexagonal, heptagonal, and octagonal,
;; is represented by a different number in the set.

(defn make-4-digits-sets [gen-fn]
  (->> (for [n (iterate inc 1)
             :let [p (gen-fn n)]
             :when (and (>= p 1000) (>= (rem p 100) 10))
             :while (< p 10000)]
         [(quot p 100) (rem p 100)])
       (apply sorted-set)))

(def triangle-set   (make-4-digits-sets (fn [n] (/ (* n (+ n 1)) 2))))
(def square-set     (make-4-digits-sets (fn [n] (* n n))))
(def pentagonal-set (make-4-digits-sets (fn [n] (/ (* n (- (* 3 n) 1)) 2))))
(def hexagonal-set  (make-4-digits-sets (fn [n] (* n (- (* 2 n) 1)))))
(def heptagonal-set (make-4-digits-sets (fn [n] (/ (* n (- (* 5 n) 3)) 2))))
(def octogonal-set  (make-4-digits-sets (fn [n] (* n (- (* 3 n) 2)))))

(defn solutions-for-seq [set-seq]
  (->> (reduce (fn [sols next-set]
                 (->> (combo/cartesian-product sols next-set)
                      (filter (fn [[partial-sol [l r]]] (== (last (last partial-sol)) l)))
                      (map (fn [[partial-sol v]] (conj partial-sol v)))
                      ))
               (map #(vector %) (first set-seq))
               (next set-seq))
       (filter (fn [sol] (== (first (first sol)) (last (last sol)))))))

(defn solution []
  (->> (combo/permutations [triangle-set square-set pentagonal-set hexagonal-set heptagonal-set octogonal-set])
       (mapcat solutions-for-seq)
       (first)
       (reduce (fn [s [l r]] (+ s r (* 100 l))) 0)
       ))

;;-> 28684