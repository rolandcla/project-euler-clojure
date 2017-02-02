(ns project-euler.problem_0127
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.set :as set])
  )

;; The radical of n, rad(n), is the product of distinct prime factors of n.
;; For example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

;; We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

;; GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
;; a < b
;; a + b = c
;; rad(abc) < c
;; For example, (5, 27, 32) is an abc-hit, because:

;; GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
;; 5 < 27
;; 5 + 27 = 32
;; rad(4320) = 30 < 32
;; It turns out that abc-hits are quite rare and there are only thirty-one abc-hits for c < 1000,
;; with ∑c = 12523.

;; Find ∑c for c < 120000.

;; From problem 0010 :
;; -------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

(defn lz-primes
  []
  (concat [2]
          (filter prime? (iterate #(+ 2 %) 3))))

;;---------------------------------------------------------------------------

(defn gcd [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

(defn rad [x]
  (loop [x x p 2 q 0 r 1]
    (cond (> (* p p) x)   (if (== x q) r (* r x))
          (< 0 (rem x p)) (recur x          (if (== 2 p) 3 (inc p)) p r)
          (== p q)        (recur (quot x p) p                       p r)
          :else           (recur (quot x p) p                       p (* r p))
          )))

(defn abc-hit? [[a b c]]
  (and (== 1 (gcd a b))
       (== 1 (gcd a c))
       (== 1 (gcd b c))
       (< a b)
       (== (+ a b) c)
       (< (rad (* a b c)) c)
       ))



(def primes (->> (lz-primes)
                 (take-while #(< % 120000))
                 (vec)
                 ))


(def rad-by-val
  (->> (range 120000)
       (map rad)
       (vec)
       ))

(def vals-by-rad
  (->> (map-indexed vector rad-by-val)
       (drop 2)
       (reduce (fn [m [k v]] (assoc m v (conj (get m v []) k)))
               (sorted-map))
       ))

(defn solution-for [c-max]
  (->> (range 3 c-max)
       (mapcat (fn [c]
                 (let [rad-c     (rad-by-val c)
                       max-rad-b (/ c rad-c)
                       min-b     (/ c 2)]
                   (->> vals-by-rad
                        (take-while (fn [[rad-b bs]] (< rad-b max-rad-b)))
                        (filter (fn [[rad-b bs]] (== 1 (gcd rad-b rad-c))))
                        (mapcat (fn [[rad-b bs]]
                                  (let [max-rad-a (/ max-rad-b rad-b)]
                                    (->> bs
                                         (drop-while (fn [b] (< b min-b)))
                                         (take-while (fn [b] (< b c)))
                                         (map (fn [b] [(- c b) b]))
                                         (filter (fn [[a b]] (let [rad-a (rad a)]
                                                               (and (< rad-a max-rad-a)
                                                                    (== 1 (gcd rad-a rad-b))))))
                                         (map (fn [[a b]] [a b c]))
                                         ))))
                     )
                   )))
       (reduce (fn [sum-c [_ _ c]] (+ sum-c c)) 0)
       ))

(defn solution []
  (solution-for 120000))

;;-> 18407904
