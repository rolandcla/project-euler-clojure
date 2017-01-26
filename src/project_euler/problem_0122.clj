(ns project-euler.problem_0122
  (:require [clojure.set :as set])
  (:require [clojure.math.combinatorics :as combo]))

;; The most naive way of computing n15 requires fourteen multiplications:

;; n × n × ... × n = n^15

;; But using a "binary" method you can compute it in six multiplications:

;; n × n = n^2
;; n^2 × n^2 = n^4
;; n^4 × n^4 = n^8
;; n^8 × n^4 = n^12
;; n^12 × n^2 = n^14
;; n^14 × n = n^15

;; However it is yet possible to compute it in only five multiplications:

;; n × n = n^2
;; n^2 × n = n^3
;; n^3 × n^3 = n^6
;; n^6 × n^6 = n^12
;; n^12 × n^3 = n^15

;; We shall define m(k) to be the minimum number of multiplications to compute n^k;
;; for example m(15) = 5.

;; For 1 ≤ k ≤ 200, find ∑ m(k).


(def intermediate-steps
  (memoize
   (fn [k]
     (if (== 1 k)
       [0 [#{}]]
       (->> (range 1 (inc (quot k 2)))
            (mapcat (fn [a]
                      (let [[s1 pss1] (intermediate-steps a)
                            [s2 pss2] (intermediate-steps (- k a))]
                        (->> (combo/cartesian-product pss1 pss2)
                             (map (fn [[ps1 ps2]] (set/union ps1 ps2 #{k})))
                             ))))
            (reduce
             (fn [[best-score best-paths] path]
               (let [s (count path)]
                 (cond (> s best-score) [best-score best-paths]
                       (< s best-score) [s          [path]]
                       :else            [best-score (conj best-paths path)]
                       )))
             [1000 nil])
            )))))

;; Very slow !!!!
(defn solution []
  (->> (range 1 201)
       (map intermediate-steps)
       (map first)
       (reduce +)))

;;-> 1582

