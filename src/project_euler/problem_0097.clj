(ns project-euler.problem-0097)

;; The first known prime found to exceed one million digits was discovered in 1999,
;; and is a Mersenne prime of the form 2^6972593−1; it contains exactly 2,098,960 digits.
;; Subsequently other Mersenne primes, of the form 2^p−1, have been found which contain more digits.

;; However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits:
;; 28433×2^7830457+1.

;; Find the last ten digits of this prime number.

(defn mult-mod [m x y]
  (rem (*' x y) m)
  )

(defn pow-mod [m x n]
  (loop [p 1, n n, e x]
    (if (zero? n)
      p
      (let [p' (if (odd? n) (mult-mod m p e) p)]
        (recur p' (quot n 2) (mult-mod m e e))
        ))))

(defn solution []
  (let [m 10000000000]
    (+ 1 (mult-mod m 28433 (pow-mod m 2 7830457)))))

;;-> 8739992577
