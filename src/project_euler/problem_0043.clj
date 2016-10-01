(ns project-euler.problem_0043
  (:require [clojure.math.combinatorics :as combo]))

;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
;; each of the digits 0 to 9 in some order,
;; but it also has a rather interesting sub-string divisibility property.

;; Let d1 be the 1st digit, d2 be the 2nd digit, ando so on.
;; In this way, we note the following:

;; d2d3d4=406 is divisible by 2
;; d3d4d5=063 is divisible by 3
;; d4d5d6=635 is divisible by 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11
;; d7d8d9=728 is divisible by 13
;; d8d9d10=289 is divisible by 17
;; Find the sum of all 0 to 9 pandigital numbers with this property.

;; from problem 0038 ----------------------------------
;;....................................................................

(defn n->digits
  [n]
  (loop [n n ds []]
    (if (== 0 n)
      (rseq ds)
      (recur (quot n 10) (conj ds (rem n 10))))))

;; from problem 0041 ----------------------------------
;;....................................................................

(defn digits->n
  [ds]
  (reduce (fn [p x] (+ x (* 10 p))) 0 ds))

;;....................................................................

(defn all-0-to-9-pandigitals
  []
  (->> (range 10)
       (combo/permutations)
       ;;(filter #(not (= 0 (first %))))
       ))

(defn subs-div-props?
  [digits]
  (and (= 0 (rem (digits->n (subvec digits 7 10)) 17))
       (= 0 (rem (digits->n (subvec digits 6 9)) 13))
       (= 0 (rem (digits->n (subvec digits 5 8)) 11))
       (= 0 (rem (digits->n (subvec digits 4 7)) 7))
       (= 0 (rem (digits->n (subvec digits 3 6)) 5))
       (= 0 (rem (digits->n (subvec digits 2 5)) 3))
       (= 0 (rem (digits->n (subvec digits 1 4)) 2))
              ))

(defn solution []
  (->> (all-0-to-9-pandigitals)
       (filter subs-div-props?)
       (map digits->n)
       (apply +)
       ))

(solution)

;;-> 16695334890
