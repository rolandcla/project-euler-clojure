(ns project-euler.problem_0103
  (:require [clojure.math.combinatorics :as combo]))

;; Let S(A) represent the sum of elements in set A of size n.
;; We shall call it a special sum set if for any two non-empty disjoint subsets, B and C,
;; the following properties are true:

;; S(B) ≠ S(C); that is, sums of subsets cannot be equal.
;; If B contains more elements than C then S(B) > S(C).
;; If S(A) is minimised for a given n, we shall call it an optimum special sum set.
;; The first five optimum special sum sets are given below.

;; n = 1: {1}
;; n = 2: {1, 2}
;; n = 3: {2, 3, 4}
;; n = 4: {3, 5, 6, 7}
;; n = 5: {6, 9, 11, 12, 13}

;; It seems that for a given optimum set, A = {a1, a2, ... , an},
;; the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b},
;; where b is the "middle" element on the previous row.

;; By applying this "rule" we would expect the optimum set for n = 6 to be A = {11, 17, 20, 22, 23, 24},
;; with S(A) = 117.
;; However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set.
;; The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25},
;; with S(A) = 115 and corresponding set string: 111819202225.

;; Given that A is an optimum special sum set for n = 7, find its set string.

;; NOTE: This problem is related to Problem 105 and Problem 106.

(defn next-near-optimum-set [xs]
  (let [middle (nth xs (quot (count xs) 2))]
    (vec (cons middle (map #(+ middle %) xs)))))

(defn is-valid? [xs]
  (let [cnt (count xs)]
    (and (->> (combo/partitions xs)
              (mapcat #(combo/combinations % 2))
              (filter (fn [[ys zs]] (== (reduce + ys) (reduce + zs))))
              (seq)
              (not)
              )
         (->> (range 1 cnt)
              (remove (fn [n] (> (reduce + (take (inc n) xs))
                                 (reduce + (drop (- cnt n) xs)))))
              (seq)
              (not)
              )
         )))

(defn search-optimum [xs]
  (let [cnt (count xs)]
    (->> (combo/selections [-3 -2 -1 0 1] cnt)
         (filter (fn [ds] (< (reduce + ds) 0)))
         (map (fn [ds] (map + xs ds)))
         (filter #(apply distinct? %))
         (map sort)
         (set)
         (filter is-valid?)
         ;;(count)
         )))

(defn solution []
  (or (->> (next-near-optimum-set [11 18 19 20 22 25])
          (search-optimum)
          (seq)
          )
      (next-near-optimum-set [11 18 19 20 22 25])))

;;-> 20 31 38 39 40 42 45
