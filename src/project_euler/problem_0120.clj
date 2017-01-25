(ns project-euler.problem_0120)

;; Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.

;; For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 ≡ 42 mod 49.
;; And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.

;; For 3 ≤ a ≤ 1000, find ∑ rmax.

(defn pow [x n]
  (loop [n n p 1]
    (if (zero? n)
      p
      (recur (dec n) (*' p x)))))

(defn sq-remainder-max [a n-max]
  (let [a-1 (- a 1)
        a+1 (+ a 1)
        a2  (* a a)]
    (loop [p-a-1 1
           p-a+1 1
           r-max 0
           n n-max]
      (if (zero? n)
        r-max
        (let [r (rem (+' p-a-1 p-a+1) a2)]
          (recur (*' p-a-1 a-1)
                 (*' p-a+1 a+1)
                 (max r r-max)
                 (dec n))))
      )
    ))

(defn brute-force-solution-for [n-max]
  (->> (range 3 1001)
       (map #(sq-remainder-max % n-max))
       (reduce +)))

(defn solution []
  (brute-force-solution-for 2200))

;;-> 333082500


