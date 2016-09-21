(ns project-euler.problem_0032
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set])
  )

;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
;; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier,
;; and product is 1 through 9 pandigital.

;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as
;; a 1 through 9 pandigital.

;; HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

(def the-9-digits (vec (range 1 10)))
(def the-9-digits-set (set the-9-digits))

(defn n->digits [n]
  (loop [n n ds '()]
    (let [ q (quot n 10)
          new-ds (cons (rem n 10) ds)]
      (if (== 0 q)
        new-ds
        (recur q new-ds))
      )))

(defn digits->n [ds]
  (loop [ds ds n 0]
    (if (seq ds)
      (recur (rest ds) (+ (first ds) (* 10 n)))
      n
      )))

(defn solution []
  (->>
   (combo/combinations the-9-digits 5)
   (filter #(< (* (first %) (second %)) 10))
   (mapcat (fn [ds]
             (->> (combo/permutations ds)
                  (filter #(< (* (first %) (second %)) 10))
                  (mapcat (fn [[a b c d e :as ds]]
                            [[ds (*  (+ (* 10 a) e) (+ (* 100 b) (* 10 c) d))]
                             [ds (*  a (+ (* 1000 b) (* 100 c) (* 10  d) e))]
                             ]
                            ))
                  (filter (fn [[ds p]] (< p 10000)))
                  (filter (fn [[ds p]] (= (clojure.set/union (set ds) (set (n->digits p)))
                                          the-9-digits-set)))
                  (reduce (fn [p-set [ds p]] (conj p-set p)) #{})
                                    )))
   (reduce +)
   ))

(solution)

;;-> 45228


