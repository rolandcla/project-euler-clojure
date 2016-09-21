(ns project-euler.problem_0016)

;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

;; What is the sum of the digits of the number 2^1000?


(defn big-int-power
  [x n]
  (nth (iterate (partial * 2N) 1) n)
  )

(defn sum-of-digits
 [n]
 (->> (str n)
      (map #(- (int %) 48))
      (reduce +)))

(defn solution []
  (sum-of-digits (big-int-power 2 1000)))

;;-> 1366

