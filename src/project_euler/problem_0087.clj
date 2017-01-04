(ns project-euler.problem_0087)

;; The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
;; In fact, there are exactly four numbers below fifty that can be expressed in such a way:

;; 28 = 2^2 + 2^3 + 2^4
;; 33 = 3^2 + 2^3 + 2^4
;; 49 = 5^2 + 2^3 + 2^4
;; 47 = 2^2 + 3^3 + 2^4

;; How many numbers below fifty million can be expressed as the sum of a prime square, prime cube,
;; and prime fourth power?

;; From problem 0010 :
;; ---------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;==========================================================================

(defn primes-under [x]
  (filter prime? (range 2 x)))

(defn solution-for [m]
  (let [primes (primes-under m)]
    (->> primes
         (map (fn [x] (* x x x x)))
         (take-while (fn [x4] (< x4 m)))
         (mapcat (fn [x4] (->> primes
                               (map (fn [y] (+ x4 (* y y y))))
                               (take-while (fn [x4+y3] (< x4+y3 m)))
                               )))
         (mapcat (fn [x4+y3] (->> primes
                                  (map (fn [z] (+ x4+y3 (* z z))))
                                  (take-while (fn [x4+y3+z2] (< x4+y3+z2 m)))
                                  )))
         (set)
         (count)
         )))

(defn solution []
  (solution-for 50000000))

;;-> 1097343
