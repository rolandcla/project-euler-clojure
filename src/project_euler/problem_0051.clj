(ns project-euler.problem_0051
  (:require [clojure.math.combinatorics :as combo]))

;; By replacing the 1st digit of the 2-digit number *3,
;; it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

;; By replacing the 3rd and 4th digits of 56**3 with the same digit,
;; this 5-digit number is the first example having seven primes among the ten generated numbers,
;; yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
;;Consequently 56003, being the first member of this family, is the smallest prime with this property.

;; Find the smallest prime which,
;; by replacing part of the number (not necessarily adjacent digits) with the same digit,
;; is part of an eight prime value family.

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;----------------------------------------------------------------------------

(def powers-of-10 (vec (take 15 (iterate #(* 10 %) 1))))

(defn replace-digit [d x n]
  (+ (rem x (powers-of-10 n))
     (* d (powers-of-10 n))
     (* (powers-of-10 (inc n)) (quot x (powers-of-10 (inc n)))))
  )

(defn replace-digits [d x ns]
  (reduce (partial replace-digit d) x ns)
  )

(defn replace-digits-and-check [x ns check?]
  (loop [nok 3 d 0]
    (if (zero? nok)
      false
      (if (== d 10)
        (some #(check? (replace-digits %1 x ns)) (range 10))
        (if (check? (replace-digits d x ns))
          (recur nok (inc d))
          (recur (dec nok) (inc d)))))
    ))

(defn solution-for-n-digits [n]
  (let [nss (next (combo/subsets (range 1 n)))
        primes (apply sorted-set (filter prime? (range (inc (powers-of-10 (dec n))) (powers-of-10 n) 2)))
        ]
    (->> (combo/cartesian-product primes nss)
         (some (fn [[x ns]] (replace-digits-and-check x ns primes)))
         )))

(solution-for-n-digits 6)

;;-> 121313

