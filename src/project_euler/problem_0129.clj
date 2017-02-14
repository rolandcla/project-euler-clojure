(ns project-euler.problem_0129)

;; A number consisting entirely of ones is called a repunit.
;; We shall define R(k) to be a repunit of length k; for example, R(6) = 111111.

;; Given that n is a positive integer and GCD(n, 10) = 1,
;; it can be shown that there always exists a value, k, for which R(k) is divisible by n,
;; and let A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.

;; The least value of n for which A(n) first exceeds ten is 17.

;; Find the least value of n for which A(n) first exceeds one-million.

(defn a [n]
  (loop [r 1 k 1]
    (let [r' (rem r n)]
      (if (zero? r')
        k
        (recur (+ 1 (* 10 r')) (inc k))
        ))))

(defn solution []
  (->> (iterate #(+ % 2) 1000001)
       (remove #(zero? (rem % 5)))
       (map (fn [n] [n (a n)]))
       (filter (fn [[n an]] (> an 1000000)))
       (first)
       (first)
       ))

;;-> 1000023
