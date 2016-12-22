(ns project-euler.problem_0072)

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1,
;; it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that there are 21 elements in this set.

;; How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

;; From problem 0070
;;-------------------
(defn step-totient [totient-set n]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [n n, m 2]
      (let [q (quot n m)]
        (cond (> m q)           (- n 1)
              (zero? (rem n m)) (if (zero? (rem q m))
                                  (* m       (totient-set q))
                                  (* (- m 1) (totient-set q)))
              :else             (recur n (next-fact m))
              )))))
;;--------------------------------------------------------------------

(defn pgcd [a b]
  (let [r (rem a b)]
    (if (zero? r)
      b
      (recur b r))))


(defn brute-force-solution [m]
  (->> (range 2 (inc m))
       (mapcat (fn [d] (for [n (range 1 d) :when (== 1 (pgcd n d))] [n d])))
       (count)))


(defn solution []
  (->> (range 2 1000001)
       (reduce (fn [ts n] (assoc ts n (step-totient ts n))) {})
       (reduce (fn [sum [n t]] (+ sum t)) 0)
       ))

;;-> 303963552391
