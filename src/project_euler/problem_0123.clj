(ns project-euler.problem_0123)

;; Let pn be the nth prime: 2, 3, 5, 7, 11, ...,
;; and let r be the remainder when (pn−1)^n + (pn+1)^n is divided by pn^2.

;; For example, when n = 3, p3 = 5, and 4^3 + 6^3 = 280 ≡ 5 mod 25.

;; The least value of n for which the remainder first exceeds 10^9 is 7037.

;; Find the least value of n for which the remainder first exceeds 10^10.

;;---------------------------------------------------------------------------

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

(defn pow-rem [x n d]
  (loop [p 1 q x n n]
    (if (zero? n)
      p
      (recur (if (odd? n)
               (rem (*' p q) d)
               p)
             (*' q q)
             (quot n 2)
             ))))

(defn prime-sq-rem [p n]
  (let [p2 (* p p)]
    (rem (+ (pow-rem (- p 1) n p2)
            (pow-rem (+ p 1) n p2))
         p2))
  )

(defn solution []
  (->> (map (fn [n pn] [n pn]) (iterate inc 1) (lz-primes))
       (drop 20500) ;; It seems that when : lim * 10 ==> n * 3
       (map (fn [[n pn]] [(prime-sq-rem pn n) n]))
       (drop-while #(<= (first %) 10000000000))
       (first)
       (second)
       ))

;;-> 21035
