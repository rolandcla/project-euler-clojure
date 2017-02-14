(ns project-euler.problem_0130)

;; A number consisting entirely of ones is called a repunit.
;; We shall define R(k) to be a repunit of length k;
;; for example, R(6) = 111111.

;; Given that n is a positive integer and GCD(n, 10) = 1,
;; it can be shown that there always exists a value, k, for which R(k) is divisible by n,
;; and let A(n) be the least such value of k;
;; for example, A(7) = 6 and A(41) = 5.

;; You are given that for all primes, p > 5, that p − 1 is divisible by A(p).
;; For example, when p = 41, A(41) = 5, and 40 is divisible by 5.

;; However, there are rare composite values for which this is also true;
;; the first five examples being 91, 259, 451, 481, and 703.

;; Find the sum of the first twenty-five composite values of n for which
;; GCD(n, 10) = 1 and n − 1 is divisible by A(n).

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

(defn a [n]
  (loop [r 1 k 1]
    (let [r' (rem r n)]
      (if (zero? r')
        k
        (recur (+ 1 (* 10 r')) (inc k))
        ))))


(defn solution []
  (->> (iterate #(+ % 2) 7)
       (remove #(zero? (rem % 5)))
       (remove prime?)
       (filter (fn [n] (zero? (rem (dec n) (a n)))))
       (take 25)
       (apply +)
       ))

;;-> 149253
