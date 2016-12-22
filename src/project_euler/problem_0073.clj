(ns project-euler.problem_0073)

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1,
;; it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;                                   ---- ---- ----
;; It can be seen that there are 3 fractions between 1/3 and 1/2.

;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?

(defn pgcd [a b]
  (let [r (rem a b)]
    (if (zero? r)
      b
      (recur b r))))


(defn brute-force-solution [m]
  (->> (range 2 (inc m))
       (mapcat (fn [d] (for [n (range 1 d) :when (== 1 (pgcd n d))] [n d])))
       (filter (fn [[n d]] (< 1/3 (/ n d) 1/2)))
       (count)
       ))

(defn solution []
  (->> (range 2 12001)
       (mapcat (fn [d]
                 (let [n-min (+ 1 (quot d 3))
                       n-max (+ 1 (quot d 2))]
                   (for [n (range n-min n-max) :when (== 1 (pgcd n d))] [n d]))))
       (count)
       (+ -1)
       ))

;;-> 7295372
