(ns project-euler.problem_0056)

;; A googol (10^100) is a massive number: one followed by one-hundred zeros;
;; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
;; Despite their size, the sum of the digits in each number is only 1.

;; Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

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

(defn pow [a b]
  (loop [n b, p 1]
    (if (zero? n)
      p
      (recur (dec n) (* p a)))))

(defn solution []
  (->> (for [a (range 1N 100N) b (range 1 100)] [a b])
       (map (fn [[a b]] (apply + (digits (pow a b)))))
       (apply max)))

;;-> 972
