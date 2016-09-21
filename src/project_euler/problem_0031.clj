(ns project-euler.problem_0031)

;; In England the currency is made up of pound, £, and pence, p,
;; and there are eight coins in general circulation:

;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;; It is possible to make £2 in the following way:

;; 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
;; How many different ways can £2 be made using any number of coins?

(defn count-ways
  [total coins]
  (cond (== 0 total) 1
        (seq coins)  (let [fst-coin (peek coins)
                           other-coins (pop coins)
                           total+ (+ total (apply min coins))]
                       (->> (for [x (range 0 total+ fst-coin)] x)
                            (map #(count-ways (- total %) other-coins))
                            (reduce +)
                            ))
        :else 0
    )
  )

(defn solution []
  (count-ways 200 [1 2 5 10 20 50 100 200]))

;;-> 73682
