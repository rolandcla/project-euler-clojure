(ns project-euler.problem_0070)

;; Euler's Totient function, φ(n) [sometimes called the phi function],
;; is used to determine the number of positive numbers less than or equal to n which are relatively prime to n.
;; For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
;; The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

;; Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

;;-------------------------------------------------------------------------------------------------------
;; from problem_0030:

(defn digits
  [n]
  (loop [n n, ds []]
    (let [r (rem n 10)
          q (quot n 10)
          new-ds (conj ds r)]
      (if (== q 0)
        new-ds
        (recur q new-ds)
        ))))
;;-------------------------------------------------------------------------------------------------------


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



(defn solution []
  (->> (range 2 10000000)
       (reduce (fn [ts n] (assoc ts n (step-totient ts n))) {})
       (filter (fn [[n t]] (= (sort (digits n)) (sort (digits t)))))
       (reduce (fn [[r-min n-min t-min] [n t]]
                 (let [r (/ n t)]
                   (if (< r r-min)
                     [r n t]
                     [r-min n-min t-min])))
               [1000 nil nil])
       (second)
       ))

;;-> 8319823
