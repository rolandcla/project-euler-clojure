(ns project-euler.problem_0050)


;; The prime 41, can be written as the sum of six consecutive primes:

;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;; This is the longest sum of consecutive primes that adds to a prime below one-hundred.

;; The longest sum of consecutive primes below one-thousand that adds to a prime,
;; contains 21 terms, and is equal to 953.

;; Which prime, below one-million, can be written as the sum of the most consecutive primes?

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;----------------------------------------------------------------------------
(def sum-max 1000000)

(def primes-vec (vec (filter prime? (range 2 sum-max))))
(def primes-set (set primes-vec))
(def n (count primes-vec))

(defn longuest-starting-at [i]
  (loop [j i, lj nil, s 0, ls nil]
    (if (and (< s sum-max) (< j n))
      (let [s (+ s (primes-vec j))]
        (if (contains? primes-set s)
          (recur (inc j) j  s s)
          (recur (inc j) lj s ls)
          ))
      [lj ls]
      )))

(defn solution []
  (loop [i 0, li 0, lj 0, ls nil]
    (if (< i n)
      (let [[tlj tls] (longuest-starting-at i)]
        (if (> (- tlj i) (- lj li))
          (recur (inc i) i  tlj tls)
          (recur (inc i) li lj  ls)
          ))
      ls
      )))

;;-> 997651
