(ns project-euler.problem_0078)

;; Let p(n) represent the number of different ways in which n coins can be separated into piles.
;; For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.

;; OOOOO
;; OOOO   O
;; OOO   OO
;; OOO   O   O
;; OO   OO   O
;; OO   O   O   O
;; O   O   O   O   O
;; Find the least value of n for which p(n) is divisible by one million.



;; Partition function (number theory)
(def p
  (memoize
   (fn [n]
     (if (zero? n)
       1N
       (->> (mapcat (fn [k] [(/ (* k (- (* 3 k) 1)) 2) (/ (* k (+ (* 3 k) 1)) 2)])
                    (iterate inc 1N))
            (take-while (fn [m] (<= m n)))
            (map (fn [m] (p (- n m))))
            (map (fn [ix pm] (if (even? (quot ix 2)) pm (- pm))) (iterate inc 0))
            (reduce +)
            )))))

(defn solution []
  (->> (iterate inc 10N)
       (filter #(zero? (rem (p %) 1000000N)))
       first))

;;-> 55374
