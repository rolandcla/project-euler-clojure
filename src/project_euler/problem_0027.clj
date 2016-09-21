(ns project-euler.problem_0027)

;; Euler discovered the remarkable quadratic formula:

;; n^2 + n + 41
;; It turns out that the formula will produce 40 primes for the consecutive integer values 0 ≤ n ≤ 39.
;; However, when n=40, 40^2 + 40 + 41 = 40 (40+1) + 41 is divisible by 41,
;; and certainly when n=41, 41^2 + 41 + 41 is clearly divisible by 41.

;; The incredible formula n^2 − 79n + 1601 was discovered,
;; which produces 80 primes for the consecutive values 0 ≤ n ≤ 79.
;; The product of the coefficients, −79 and 1601, is −126479.

;; Considering quadratics of the form:

;; n^2 + an + b, where |a| < 1000 and |b| ≤ 1000

;; where |n| is the modulus/absolute value of n
;; e.g. |11| = 11 and |−4| = 4
;; Find the product of the coefficients, a and b,
;; for the quadratic expression that produces the maximum number of primes for consecutive values of n,
;; starting with n=0.

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;....................................................................


(defn consecutive-primes
  [a b]
  (loop [x 0]
    (let [y (+ (* x x) (* a x) b)]
      (if (and (> y 1) (prime? y))
        (recur (inc x))
        x
        ))))

(defn max-number-of-primes
  [n]
  (->> (for [b (range 1 n),
             a (range (- b) n)]
         [a b])

       (map (fn [[a b]] [(consecutive-primes a b) a b]))
       (reduce #(if (>  (first %2) (first %1)) %2 %1))
       ;;((fn [[n a b]] [n (* a b)]))
       ))

(defn solution []
  (let [[n a b] (max-number-of-primes 1000)]
    (* a b)
    )
  )


;;-> [71 -59231]
;;       ------

