(ns project-euler.problem_0034
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set]
            ))

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(defn factorial [n] (reduce *' (range 1 (inc n))))

(def fact (vec (map factorial (range 10))))

(defn next-digit
  [r-min r-max]
  (->> (range 10)
       (filter #(<= r-min (fact %) r-max))
       )
  )

(defn n-digits-solutions
  [n r-min r-max]
  (if (== 0 n)
    [[]]
    (mapcat
     (fn [d] (map #(conj % d) (n-digits-solutions (dec n) (- r-min (fact d)) (- r-max (fact d)))))
     (next-digit r-min r-max)
     )))

(defn solution []
  (->> (range 2 6)
       (mapcat (fn [n] (n-digits-solutions n (Math/pow 10 (dec n)) (dec (Math/pow 10 n)))))
       (map (fn [ds] [ds (reduce #(+ %1 (fact %2)) 0 ds)]))
       (mapcat (fn [[ds f-sum]] (->> (combo/permutations ds)
                                 (map (fn [pds] (reduce #(+ %2 (* 10 %1)) 0 pds)))
                                 (filter #(= % f-sum))
                                 )))
       (set)
       (reduce +)
       )
  )

(solution)

;;-> 40730

