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

(def primes (->> (lz-primes)
                 (take-while #(< % 120000))
                 (vec)
                 ))

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

(defn decompose [x]
  (loop [x x
         ps primes
         q 0
         r 1
         upfs []
         mpfs []]
    (let [p (first ps)]
      (cond (> (* p p) x)   (if (== x q)
                              [r upfs (conj mpfs x)]
                              [(* r x) (conj upfs x) mpfs])
            (< 0 (rem x p)) (recur x          (rest ps) p r        upfs          mpfs)
            (== p q)        (recur (quot x p) ps        p r        upfs          (conj mpfs p))
            :else           (recur (quot x p) ps        p (* r p) (conj upfs p)  mpfs)
            ))))




(defn naive-solution-for [c-max]
  (->> (range 3 c-max)
       (mapcat (fn [c]
                 (->> (if (odd? c) (iterate inc 1) (iterate #(+ % 2) 1))
                      (map (fn [a] [a (- c a) c]))
                      (take-while (fn [[a b c]] (< a b)))
                      )))
       (filter (fn [[a b c]] (and (== 1 (gcd a b))
                                  (== 1 (gcd a c))
                                  ;;(== 1 (gcd b c))
                                  (< (rad (* a b c)) c)
                                  )))
       (map (fn [[a b c]] c))
       (reduce +)
       ;;(count)
       ))


(defn products-under [pfs p-max r-max p r]
  ;;(println pfs p-max r-max p r)
  (let [[x & pfs'] pfs]
    (if x
      (let [r' (* r x)]
        (concat
         (when (< r' r-max)
           (->> (iterate #(* % x) (* p x))
                (take-while #(< % p-max))
                (mapcat (fn [p'] (cons [p' r'] (products-under pfs' p-max r-max p' r'))))
                ))
         (products-under pfs' p-max r-max p r)
         ))
      nil
      )))

(defn factors-with-product-under [xs p-max p-fs]
  (let [p-fs' (for [[p fs] p-fs
                    x      xs
                    :let [p' (* x p)]
                    :while (< p' p-max)]
                [p' (conj fs x)])]
    (when (seq p-fs')
      (concat p-fs'
              (factors-with-product-under (next xs) p-max p-fs')
       ))
    )
  )

(defn factors-with-product-under [xs p-max]
  (reduce
   (fn [p-fs x]
     (->> (concat p-fs
                  (for [[p fs] p-fs
                        :let [p' (* p x)]
                        :while (< p' p-max)
                        ]
                    [p' (conj fs x)])
                  )
          (sort)
          ))
   [[1 []]]
   xs
   ))


(defn solutions-for-ab [c r c-pfs]
  (let [ab-pfs (->> primes
                    (take-while #(< % (/ c r)))
                    (filter #(== -1 (.indexOf c-pfs %)))
                    )]
    ;;(println ab-pfs)
    (->> (products-under ab-pfs c (/ c r) 1 1)
         (mapcat (fn [[b r-b]]
                   (let [a (- c b)]
                     (when (and (< a b)
                                (== 1 (gcd a b))
                                (< (rad (* a b c)) c))
                       [[a b c]]))))
         )))

(defn solution-for [c-max]
  (->> (range 3 c-max)
       (map (fn [c] [c (decompose c)]))
       (filter (fn [[c [r upfs mpfs]]] (seq mpfs)))
       (mapcat (fn [[c [r upfs mpfs]]]
                 (->> (solutions-for-ab c r upfs)
                      )))
       (map (fn [[a b c]] c))
       (reduce +)
       ))


(defn unique-prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs #{}]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))


(def prime-factors
  (->> (range 120000)
       (map unique-prime-factors)
       (vec)
       ))

(defn solutions-for-c [c]
  (let [c-pfs (nth prime-factors c)]
    (->> (for [a (range 1 (/ c 2) (if (even? c) 2 1))
               :let [b (- c a)]]
           [a b c])
         (filter (fn [[a b c]]
                   (let [a-pfs (nth prime-factors a)
                         b-pfs (nth prime-factors b)]
                     (and (empty? (set/intersection a-pfs b-pfs c-pfs))
                          (< (apply * (set/union a-pfs b-pfs c-pfs)) c)))
                   ))
         )))

(defn solution []
  (->> (range 3 1000)
       (mapcat (fn [c] (solutions-for c)))
       ;;(count)
       (map (fn [[a b c]] c))
       (reduce +)
       ))
