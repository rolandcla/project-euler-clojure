(ns project-euler.problem-0086
  (:require [clojure.math.combinatorics :as combo]))

;; A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3,
;; and a fly, F, sits in the opposite corner.
;; By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10
;; and the path is shown on the diagram.

;; However, there are up to three "shortest" path candidates for any given cuboid
;; and the shortest route doesn't always have integer length.

;; It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer dimensions,
;; up to a maximum size of M by M by M, for which the shortest route has integer length when M = 100.
;; This is the least value of M for which the number of solutions first exceeds two thousand;
;; the number of solutions when M = 99 is 1975.

;; Find the least value of M such that the number of solutions first exceeds one million.



;; From problem 0075
;;------------------
(defn prime-factors
  [n]
  {:pre [(integer? n) (> n 0)]} 

  (letfn
   [(next-fact [n] (if (== n 2) 3 (+ n 2)))]

   (loop [n n, m 2, factors []]
     (let [mmax (+ (int (Math/sqrt n)) 1)]
       (cond (== 1 n)         factors
             (> m mmax)       (conj factors n)
             (== 0 (rem n m)) (recur (/ n m) m (conj factors m))
             :else            (recur n (next-fact m) factors) )))))

(defn dickson
  "https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples"
  [r]
  (let [st  (/ (* r r) 2)
        pfs (prime-factors st)]
    (->> (combo/subsets pfs)
         (filter seq)
         (map (fn [xs] (let [p (apply * xs)] [p (/ st p)])))
         (reduce (fn [s-t-set [s t]]
                   (conj s-t-set (if (< s t) [s t] [t s])))
                 #{})
         (map (fn [[s t]] [(+ r s) (+ r t) (+ r s t)]))
         )))
;;-------------------------------------------------------------------------------

(defn sq [x] (* x x))

(defn solution-for1 [m]
  (->> (for [x (range 1 (inc m)), y (range 1 (inc x)), z (range 1 (inc y))] [x y z])
       (filter (fn [[x y z]]
                 (let [s-path (Math/sqrt (+ (sq x) (sq (+ y z))))]
                   (== s-path (int s-path)))))
       ;;(count)
       ))

(defn solution-for [m]
  (->> (range 2 m 2)
       (mapcat dickson)
       (filter (fn [[a b _]] (and (<= a m) (<= (/ b 2) m))))
       (mapcat (fn [[a b _]] [[a b] [b a]]))
       (mapcat (fn [[x y+z]] (concat (for [y (range 1 (inc (quot y+z 2)))
                                           :let [z (- y+z y)]
                                           :when (and (<= y x) (<= z x))]
                                       (sort [x y z]))
                                     )))

       (filter (fn [[x y z]] (<= x y z m)))
       (count)
       ))

(defn solution []
  (->> (range 1800 1900)
       (drop-while (fn [m] (<= (solution-for m) 1000000)))
       (first)
       ))

;;-> 1818
