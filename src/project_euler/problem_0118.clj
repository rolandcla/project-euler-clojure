(ns project-euler.problem_0118
  (:require [clojure.math.combinatorics :as combo]))

;; Using all of the digits 1 through 9 and concatenating them freely to form decimal integers,
;; different sets can be formed.
;; Interestingly with the set {2,5,47,89,631}, all of the elements belonging to it are prime.

;; How many distinct sets containing each of the digits one through nine exactly once
;; contain only prime elements?

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (and (not (>= 1 n))
       (or (== 2 n)
           (let [mmax (+ (Math/sqrt n) 1)]
             (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
             ))))

(def mz-prime?
  (memoize prime?))

(defn contains-multiple-of-3? [dss]
  (some (fn [ds]
          (and (not (= ds [3]))
               (zero? (rem (reduce + ds) 3))
               ))
        dss))

(defn contains-multiple-of-2-or-5? [dss]
  (some (fn [ds]
          (and (not (= ds [2]))
               (not (= ds [5]))
               (let [last-d (last ds)]
                 (or (even? last-d) (== 5 last-d)))
               ))
        dss))

(defn digit-permutations [dss]
  (->> dss
       (filter (fn [ds]
                 (or (= ds [3])
                     (not (zero? (rem (reduce + ds) 3)))
                     )))
       (map combo/permutations)
       (apply combo/cartesian-product)
       ))

(defn apply-digits [ds]
  (reduce (fn [x d] (+ d (* 10 x))) 0 ds))

(defn solution []
  (->> (range 1 10)
       (combo/partitions)
       (remove #(some #{[1] [4] [6] [8]} %))
       (remove contains-multiple-of-3?)
       (mapcat digit-permutations)
       (remove contains-multiple-of-2-or-5?)
       (filter (fn [dss]
                 (every? (fn [ds] (mz-prime? (apply-digits ds)))
                         dss)))
       (count)
       ))

;;-> 44680
