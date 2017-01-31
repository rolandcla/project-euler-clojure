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

(defn abc-hit? [[a b c]]
  (and (== 1 (gcd a b))
       (== 1 (gcd a c))
       (== 1 (gcd b c))
       (< a b)
       (== (+ a b) c)
       (< (rad (* a b c)) c)
       ))

(defn gcd [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

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


(defn rad [x]
  (loop [x x p 2 q 0 r 1]
    (cond (> (* p p) x)   (if (== x q) r (* r x))
          (< 0 (rem x p)) (recur x          (if (== 2 p) 3 (inc p)) p r)
          (== p q)        (recur (quot x p) p                       p r)
          :else           (recur (quot x p) p                       p (* r p))
        )))



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

;; (defn solution-for [c-max]
;;   (->> (range 3 c-max)
;;        (map (fn [c] [c (decompose c)]))
;;        (filter (fn [[c [r upfs mpfs]]] (seq mpfs)))
;;        ;;(filter (fn [[c [r upfs mpfs]]] (>= (/ c r) 6)))
;;        ;;(count)
;;        (mapcat (fn [[c [r upfs mpfs]]]
;;                  (->> (if (odd? c) (iterate inc 1) (iterate #(+ % 2) 1))
;;                       (map (fn [a] [a (- c a) c]))
;;                       (take-while (fn [[a b c]] (< a b)))
;;                       (remove (fn [[a b c]]
;;                                 (some #(or (zero? (rem a %))
;;                                            (zero? (rem b %))) upfs)
;;                                 ))
;;                       )
;;                  ))
;;        (filter (fn [[a b c]] (< (rad (* a b c)) c)))
;;        (map (fn [[a b c]] c))
;;        (reduce +)
;;        ))

;; (defn solutions-for-a [rad-ab c-pfs]
;;   (let [ab-pfs (->> primes
;;                     (take-while #(< % rad-ab))
;;                     (filter #(== -1 (.indexOf c-pfs %)))
;;                     )]
;;     (products-under rad-ab ab-pfs)))

(defn products-under [p-max xs]
  (let [[x & xs'] xs]
    (if x
      (->> (iterate (fn [[p p-max']] [(* p x) (/ p-max' x)]) [1 p-max])
                (take-while (fn [[p p-max']] (< p p-max)))
                (mapcat (fn [[p p-max']]
                          (map #(* p %) (products-under p-max' xs'))))
                )
      [1]
      )))

(defn solutions-for-ab [c r c-pfs]
  (let [ab-pfs (->> primes
                    (take-while #(< % (/ c r)))
                    (filter #(== -1 (.indexOf c-pfs %)))
                    )]
    (println ab-pfs)
    (->> (products-under (/ c r) ab-pfs)
         (mapcat (fn [pb]
                   (->> ab-pfs
                        (filter #(and (not (zero? (rem pb %)))
                                      (< % (/ c r (rad pb)))
                                      ))
                        (products-under pb)
                        (map (fn [pa] [pa pb]))
                        (filter (fn [[a b]] (== c (+ a b))))
                        (map (fn [[a b]] [a b c]))
                        )
                   ))
         )))


;; (defn prime-factors-pairs [p-max xs]
;;   (->> (combo/subsets xs)
;;        (filter seq)
;;        (filter #(<= (apply * %) p-max))
;;        (mapcat (fn [a-ss]
;;                  (->> (set/difference (set xs) (set a-ss))
;;                       (vec)
;;                       (combo/subsets)
;;                       (filter seq)
;;                       (filter #(<= (apply * %) p-max))
;;                       (map (fn [b-ss] [a-ss b-ss]))
;;                       )))
;;        ))

;; (defn solutions-for-ab [p-max c-pfs]
;;   (let [ab-pfs (->> primes
;;                     (take-while #(< % p-max))
;;                     (filter #(== -1 (.indexOf c-pfs %)))
;;                     )]
;;     (->> (prime-factors-pairs p-max ab-pfs)
;;          (mapcat (fn [[a-ss b-ss]]
;;                    (->> (products-under p-max b-ss)
;;                         (mapcat (fn [pb]
;;                                   (->> (products-under pb a-ss)
;;                                        (map (fn [pa] [pa pb])))))
;;                         )))
;;          )))



(defn solution-for [c-max]
  (->> (range 3 c-max)
       (map (fn [c] [c (decompose c)]))
       (filter (fn [[c [r upfs mpfs]]] (seq mpfs)))
       (mapcat (fn [[c [r upfs mpfs]]]
                 (->> (solutions-for-ab c r upfs)
                      )))
       ;;(drop 20)
       (remove abc-hit?)
       (take 10)
       ;;(count)
       
       ;;(filter (fn [[a b c]] (< (rad (* a b c)) c)))
       ;;(map (fn [[a b c]] c))
       ;;(reduce +)
       ))


