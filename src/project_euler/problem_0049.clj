(ns project-euler.problem_0049
  (:require [clojure.math.combinatorics :as combo]))

;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
;; is unusual in two ways:
;; (i) each of the three terms are prime, and,
;; (ii) each of the 4-digit numbers are permutations of one another.

;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
;; but there is one other 4-digit increasing sequence.

;; What 12-digit number do you form by concatenating the three terms in this sequence?

;;-------------------------------------------------------------------------------------------------------
;; from problem_0030:

(defn digits
  [n]
  (loop [n n, ds []]
    (let [r (rem n 10)
          q (quot n 10)
          new-ds (conj ds r)]
      (if (== q 0)
        new-ds
        (recur q new-ds)
        ))))

;;-------------------------------------------------------------------------------------------------------
;; from problem_0007:

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;-------------------------------------------------------------------------------------------------------

(def four-digits-primes (filter prime? (range 1001 10000 2)))

(defn from-digits [digits] (reduce #(+ %2 (* 10 %1)) 0 digits))

(defn solution []
  (->> four-digits-primes
       (group-by (comp from-digits sort digits))
       (vals)
       (filter #(>= (count %) 3))
       (mapcat (fn [xs] (->> (combo/combinations xs 2)
                          (group-by (fn [[x y]] (- y x)))
                          (vals)
                          (filter #(>= (count %) 2))
                          )))
       (map #(apply concat %))
       (remove #(== (count %) (count (distinct %))))
       (remove #(some #{1487} %))
       (first)
       (distinct)
       (apply str)
       ))

;; 296962999629
