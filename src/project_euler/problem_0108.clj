(ns project-euler.problem_0108
  (:require [clojure.math.combinatorics :as combo]))

;; n the following equation x, y, and n are positive integers.

;; 1   1   1
;; - + - = -
;; x   y   n

;; For n = 4 there are exactly three distinct solutions:

;; 1    1   1
;; - + -- = -
;; 5   20   4

;; 1    1   1
;; - + -- = -
;; 6   12   4

;; 1   1   1
;; - + - = -
;; 8   8   4

;; What is the least value of n for which the number of distinct solutions exceeds one-thousand?

;; NOTE: This problem is an easier version of Problem 110;
;; it is strongly advised that you solve this one first

(defn sol-optic-equation [m n k]
  [(* k m (+ m n))
   (* k n (+ m n))
   (* k m n)])

(defn fn-gen-coprimes [mns]
  (mapcat (fn [[m n]]
            [[(- (* 2 m) n) m]
             [(+ (* 2 m) n) m]
             [(+ m (* 2 n)) n]])
          mns))

(defn gen-coprimes [p-max]
  (->>
   (loop [mns [[2 1] [3 1]]
          cps []]
     (let [mns' (filter #(<= (apply * %) p-max) mns)]
       (if (seq mns')
         (recur (fn-gen-coprimes mns') (vec (concat cps mns')))
         cps)))
   (sort-by #(apply * %))))


(defn coprimes-by-product [p-max]
  (->> (gen-coprimes p-max)
       (group-by #(apply * %))
       ;;(sort-by #(Math/pow (first %) (/ (count (second %)))))
       ))

(def the-first-primes
  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97])

(defn search-solution [p-max]
  (let [cps (coprimes-by-product p-max)]
    (->> the-first-primes
         (take 2)
         ((fn [ps] (mapcat (fn [n] (combo/combinations
                                    (flatten (repeat n ps))
                                    n ))
                           (range 1 11))))
         ;;(remove empty?)
         (map #(apply * %))
         (filter #(< % p-max))
         (sort)
         (mapcat cps)
         (map (fn [[m n]] (sol-optic-equation m n 1)))
         ;;(map count)
         ;;(reduce +)
         
         )))

;; Il faudrait trouver le ppcm de 334 nombres (coprimes)

(defn prime-factors [n p-max]
  (->> the-first-primes
       (take n)
       (mapcat (fn [p]
                 (loop [ps [] pp p]
                   (if (> pp p-max)
                     ps
                     (recur (conj ps p) (* pp p))))))
       ))

(defn count-products [n p-max]
  (let [pfs (prime-factors n p-max)
        cnt (->> pfs
                 (combo/subsets)
                 (remove empty?)
                 (count)
                 )
        lcm (reduce *' pfs)]
    [cnt lcm pfs]))

(defn count-products2 [pfs-map]
  (let [pfs (->> pfs-map
                 (mapcat (fn [[p n]] (repeat n p))))
        p   (reduce * pfs)
        cps (coprimes-by-product  p)
        ]
    pfs
    [p
     (->> pfs
          (combo/subsets)
          (remove empty?)
          (map #(apply * %))
          (mapcat cps)
          ;;(set)
          (count)
          ;; (mapcat (fn [[m n]] [(sol-optic-equation m n (/ p m n))
          ;;                      (sol-optic-equation n m (/ p m n))]))
          ;; (set)
          ;; (count)
          )]
    ))

(defn best-lcm [pfs-map]
  (let [pfs (->> pfs-map
                 (mapcat (fn [[p n]] (repeat n p))))]
    (->> pfs
         (combo/count-subsets)
         )
    )
  )

(defn search-best-prime-factors []
  (->> (range 3 7)
       (map (fn [n]
              (loop [l 5 h 5000]
                ;;(println l h)
                (if (<= (- h l) 1)
                  [n h (count-products n h)]
                  (let [m (quot (+ l h) 2)
                        [cnt _ _] (count-products n m)]
                    (if (> cnt 333)
                      (recur l m)
                      (recur m h)))))))
       (sort-by (fn [[n p-max [cnt lcm]]] lcm))
       ))


(defn check-solution []
  (let [[n p-max [cnt lcm pfs]] (first (search-best-prime-factors))
        cps (coprimes-by-product 1000)]
    (->> pfs
         (combo/subsets)
         (remove empty?)
         (map #(apply * %))
         (sort)
         (take-while #(< % 1000))
         (mapcat cps)
         (count)
         )
    ))

(defn solution []
  (search-best-prime-factors))
