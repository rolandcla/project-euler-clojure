(ns project-euler.problem_0076
  (:require [clojure.math.combinatorics :as combo]))

;; It is possible to write five as a sum in exactly six different ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can one hundred be written as a sum of at least two positive integers?

(defn sum-generator [n]
  (let [ones (repeat n 1)]
    (->> (combo/partitions ones :min 2)
         (map (fn [s] (map count s)))
         )))

;; Partition function (number theory)
(def p
  (memoize
   (fn [n]
     (if (zero? n)
       1
       (->> (mapcat (fn [k] [(/ (* k (- (* 3 k) 1)) 2) (/ (* k (+ (* 3 k) 1)) 2)])
                    (iterate inc 1))
            (take-while (fn [m] (<= m n)))
            (map (fn [m] (p (- n m))))
            (map (fn [ix pm] (if (even? (quot ix 2)) pm (- pm))) (iterate inc 0))
            (reduce +)
            )))))

(defn solution []
  (- (p 100) 1))

;;-> 190569291
