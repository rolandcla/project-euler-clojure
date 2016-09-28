(ns project-euler.problem_0041
  (:require [clojure.math.combinatorics :as combo]))

;; We shall say that an n-digit number is pandigital if
;; it makes use of all the digits 1 to n exactly once.
;; For example, 2143 is a 4-digit pandigital and is also prime.

;; What is the largest n-digit pandigital prime that exists?

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;; from problem 0038 ----------------------------------
;;....................................................................

(defn n->digits
  [n]
  (loop [n n ds []]
    (if (== 0 n)
      (rseq ds)
      (recur (quot n 10) (conj ds (rem n 10))))))

;;....................................................................

(defn digits->n
  [ds]
  (reduce (fn [p x] (+ x (* 10 p))) 0 ds))

(defn solution []
  (->> (range 9 3 -1)
       (map #(range % 0 -1))
       (mapcat combo/permutations)
       (map digits->n)
       (filter prime?)
       (first)
       ))

(solution)

;;-> 7652413


