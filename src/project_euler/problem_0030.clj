(ns project-euler.problem_0030)

;; Surprisingly there are only three numbers that can be written as
;; the sum of fourth powers of their digits:

;; 1634 = 14 + 64 + 34 + 44
;; 8208 = 84 + 24 + 04 + 84
;; 9474 = 94 + 44 + 74 + 44
;; As 1 = 14 is not a sum it is not included.

;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;; Find the sum of all the numbers that can be written as
;; the sum of fifth powers of their digits.

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

(defn sum-of-powers
  [powers n]
  (->> (digits n)
       (map powers)
       (reduce +)
       ))

(defn numbers-equals-n-power-digits
  [p]
  (let [powers (apply vector (for [d (range 10)] (int (Math/pow d p))))
        max-n (sum-of-powers powers (- (int (Math/pow 10 p)) 1))]
    (->> (range 2 max-n)
         (filter #(== % (sum-of-powers powers %)))
         )))

(defn solution []
  (->> (numbers-equals-n-power-digits 5)
       (reduce +)
       ))

;;-> 443839




