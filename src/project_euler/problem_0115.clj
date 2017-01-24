(ns project-euler.problem_0115
  (:require [clojure.math.combinatorics :as combo]))

;; NOTE: This is a more difficult version of Problem 114.

;; A row measuring n units in length has red blocks with a minimum length of m units placed on it,
;; such that any two red blocks (which are allowed to be different lengths)
;; are separated by at least one black square.

;; Let the fill-count function, F(m, n), represent the number of ways that a row can be filled.

;; For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

;; That is, for m = 3, it can be seen that n = 30 is the smallest value for which
;; the fill-count function first exceeds one million.

;; In the same way, for m = 10, it can be verified that F(10, 56) = 880711
;; and F(10, 57) = 1148904,
;; so n = 57 is the least value for which the fill-count function first exceeds one million.

;; For m = 50, find the least value of n for which the fill-count function first exceeds one million.

;;==================================================================

;; From problem 0114 :
;;------------------------------------------------------------------
(defn compositions [n m]
  (if (< n m)
    [[]]
    (concat (compositions n (inc m))
            (map (fn [cs] (conj cs m))
                 (compositions (- n m 1) m))
            )))

(def mz-count-positions
  (memoize
   (fn [n blks]
     (if (empty? blks)
       1
       (let [nb (count blks)
             sum (reduce + blks)
             [b & bs] blks]
         (->> (range (- n sum nb -2))
              (map (fn [i] (mz-count-positions (- n b i 1) bs)))
              (reduce +')
              ))))))


(defn count-ways [n m]
  (->> (compositions n m)
       (map (fn [blks]
              (*' (combo/count-permutations blks)
                  (mz-count-positions n blks))))
       (reduce +')))

;;-------------------------------------------------------------------

(defn solution []
  (->> (iterate inc 50)
       (filter (fn [n] (> (count-ways n 50) 1000000)))
       (first)
       ))

;;-> 168
