(ns project-euler.problem-0075
  (:require [clojure.math.combinatorics :as combo]))

;; It turns out that 12 cm is the smallest length of wire that can be bent to form
;; an integer sided right angle triangle in exactly one way, but there are many more examples.

;; 12 cm: (3,4,5)
;; 24 cm: (6,8,10)
;; 30 cm: (5,12,13)
;; 36 cm: (9,12,15)
;; 40 cm: (8,15,17)
;; 48 cm: (12,16,20)

;; In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle,
;; and other lengths allow more than one solution to be found;
;; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.

;; 120 cm: (30,40,50), (20,48,52), (24,45,51)

;; Given that L is the length of the wire,
;; for how many values of L ≤ 1,500,000 can exactly one integer sided right angle triangle be formed?

;; From problem 0003
;;-------------------
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

;;------------------------------------------------------------------------

(defn dickson [r]
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

(defn brute-force-solution []
  (->> (range 2 375000 2)
       (mapcat dickson)
       (map (fn [xyz] [(apply + xyz) xyz]))
       (filter (fn [[perim _]] (<= perim 1500000)))
       (group-by (fn [[perim _]] perim))
       (filter (fn [[perim xyzs]] (== 1 (count xyzs))))
       (count)
       ))
