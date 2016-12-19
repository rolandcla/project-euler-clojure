(ns project-euler.problem_0063)

;; The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

;; How many n-digit positive integers exist which are also an nth power?

(defn count-digits [x]
  (loop [x x, n 1]
    (let [q (quot x 10)]
      (if (zero? q)
        n
        (recur q (inc n))))))

(defn pow [x n]
  (loop [n n p 1]
    (if (zero? n)
      p
      (recur (dec n) (*' p x)))))

(defn solution []
  (->> (range 1 100)
       (reduce
        (fn [cnt n]
          (->> (map (fn [x] (pow x n)) (range 1 10))
               (filter (fn [x_n] (== (count-digits x_n) n)))
               (count)
               (+ cnt)
               )) 0)
       ))

;;-> 49
