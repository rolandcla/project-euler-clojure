(ns project-euler.problem_0134)

;; Consider the consecutive primes p1 = 19 and p2 = 23.
;; It can be verified that 1219 is the smallest number such that
;; the last digits are formed by p1 whilst also being divisible by p2.

;; In fact, with the exception of p1 = 3 and p2 = 5, for every pair of consecutive primes, p2 > p1,
;; there exist values of n for which the last digits are formed by p1 and n is divisible by p2.
;; Let S be the smallest of these values of n.

;; Find ∑ S for every pair of consecutive primes with 5 ≤ p1 ≤ 1000000.

;; From problem 0010 :
;; -------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))
;;------------------------------------------------------------------------

(def primes-under-1e6
  (->> (range 5 1000004)
       (filter prime?)))

(defn last-digit [x]
  (mod x 10))

(defn connect-last-digit [p1 p2]
  (let [d1 (last-digit p1)]
    (->> (range 10)
         (map #(vector % (* p2 %)))
         (filter #(== d1 (last-digit (second %))))
         (first)
         )))

(defn mod- [x y]
  (let [q (loop [p10 10] (if (< x p10) p10 (recur (* p10 10))))]
    (mod (- x y) q)))

(defn from-digits [ds]
  (reduce (fn [p d] (+ d (* p 10))) 0 ds))

(defn connect-primes [p1 p2]
  (loop [p1 p1 r1 0 ss []]
    (if (zero? p1)
      (* p2 (from-digits (rseq ss)))
      (let [[n p1'] (connect-last-digit (mod- p1 r1) p2)]
        (recur (quot p1 10) (quot (+ p1' r1) 10) (conj ss n))
        )
      )
    ))

(defn check-connection [p1 p2 s]
  (and (loop [p1 p1 s s]
         (cond (zero? p1) true
               (== (last-digit p1) (last-digit s)) (recur (quot p1 10) (quot s 10))
               :else false
               ))
       (zero? (rem s p2))))

(defn solution []
  (->> (map vector primes-under-1e6 (rest primes-under-1e6))
       (map (fn [[p1 p2]] (connect-primes p1 p2)))
       (reduce +)
       ))

;;-> 18613426663617118
