(ns project-euler.problem_0111
  (:require [clojure.math.combinatorics :as combo]))

;; Considering 4-digit primes containing repeated digits it is clear that they cannot all be the same:
;; 1111 is divisible by 11, 2222 is divisible by 22, and so on.
;; But there are nine 4-digit primes containing three ones:

;; 1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111

;; We shall say that M(n, d) represents the maximum number of repeated digits
;; for an n-digit prime where d is the repeated digit,
;; N(n, d) represents the number of such primes, and S(n, d) represents the sum of these primes.

;; So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime where one is the repeated digit,
;; there are N(4, 1) = 9 such primes, and the sum of these primes is S(4, 1) = 22275.
;; It turns out that for d = 0, it is only possible to have M(4, 0) = 2 repeated digits,
;; but there are N(4, 0) = 13 such cases.

;; In the same way we obtain the following results for 4-digit primes.

;;  Digit, d | M(4, d) | N(4, d) | S(4, d)|
;;  ---------|---------|---------|--------|
;;    0      |    2    |    13   |  67061 |
;;    1      |    3    |     9   |  22275 |
;;    2      |    3    |     1   |   2221 |
;;    3      |    3    |    12   |  46214 |
;;    4      |    3    |     2   |   8888 |
;;    5      |    3    |     1   |   5557 |
;;    6      |    3    |     1   |   6661 |
;;    7      |    3    |     9   |  57863 |
;;    8      |    3    |     1   |   8887 |
;;    9      |    3    |     7   |  48073 |
;;  =======================================

;; For d = 0 to 9, the sum of all S(4, d) is 273700.

;; Find the sum of all S(10, d).

;;=====================================================================================

(defn odd-prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (let [mmax (+ (Math/sqrt n) 1)]
    (not (some #(== 0 (rem n %)) (range 7 mmax 2)))
    ))


(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))

(defn primes-with-repeated-digits [n d]
  (let [ds (vec (repeat n d))]
    (loop [n-m 1]
      (let [cs (apply combo/cartesian-product (repeat n-m (for [e (range 10) :when (not (== e d))] e)))
            ps (->> (combo/combinations (range n) n-m)
                    (mapcat (fn [ixs] (map (fn [vs] [ixs vs]) cs)))
                    (map (fn [[ixs vs]] (reduce (fn [ds' [ix v]] (assoc ds' ix v))
                                                ds
                                                (map vector ixs vs))))
                    (remove (fn [ds]
                              (let [last-d (nth ds (dec n))]
                                (or (even? last-d)
                                    (== 5 last-d)
                                    (== 0 (rem (reduce + ds) 3))
                                    (== 0 (nth ds 0))
                                    ))))
                    (map (fn [ds] (reduce (fn [x d] (+ d (* 10 x))) 0 ds)))
                    (filter odd-prime?)
                    )]
        (if (seq ps)
          [(- n n-m) ps]
          (recur (inc n-m)))
        ))))

(defn solution-for-n-digits [n]
  (->> (range 10)
       (map (fn [d] [d (primes-with-repeated-digits n d)]))
       (map (fn [[d [m ps]]] [d m (count ps) (reduce + ps)]))
       ))

(defn solution []
  (->> (solution-for-n-digits 10)
       (map last)
       (reduce +)
       ))

;;-> 612407567715
