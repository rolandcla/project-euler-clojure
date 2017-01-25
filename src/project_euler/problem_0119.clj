(ns project-euler.problem_0119)

;; The number 512 is interesting because it is equal to the sum of its digits raised to some power:
;; 5 + 1 + 2 = 8, and 83 = 512.
;; Another example of a number with this property is 614656 = 284.

;; We shall define an to be the nth term of this sequence
;; and insist that a number must contain at least two digits to have a sum.

;; You are given that a2 = 512 and a10 = 614656.

;; Find a30.

(defn sum-of-digits [x]
  (loop [x x s 0]
    (if (zero? x)
      s
      (recur (quot x 10) (+ s (rem x 10))))))

(defn power-of? [x b]
  (loop [x x]
    (or (== x b)
        (and (zero? (rem x b))
             (recur (quot x b))))))

(defn pow [x n]
  (loop [n n p 1]
    (if (zero? n)
      p
      (recur (dec n) (* p x)))))

(defn n-digit-powers [n]
  (let [n-d-min (pow 10 (dec n))
        n-d-max (pow 10 n)]
    (->> (range 2 (inc (* 9 n)))
         (mapcat (fn [b]
                   (->> (iterate inc 2)
                        (map (fn [m] (pow b m)))
                        (drop-while #(< % n-d-min))
                        (take-while #(< % n-d-max))
                        )))
         (sort)
         (dedupe)
         )))


(defn solution []
  (->> (iterate inc 2)
       (mapcat n-digit-powers)
       (map (fn [x] [x (sum-of-digits x)]))
       (remove (fn [[x b]] (== 1 b)))
       (filter (fn [[x b]] (power-of? x b)))
       (take 30)
       (last)
       (first)
       ))

;;-> 248155780267521
