(ns project-euler.problem_0040)

;; An irrational decimal fraction is created by concatenating the positive integers:

;; 0.123456789101112131415161718192021...

;; It can be seen that the 12th digit of the fractional part is 1.

;; If dn represents the nth digit of the fractional part,
;; find the value of the following expression.

;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(defn digits
  [n]
  (loop [n n, ds []]
    (let [r (rem n 10)
          q (quot n 10)
          new_ds (conj ds r)]
      (if (== 0 q)
        (rseq  new_ds)
        (recur q new_ds)))
    )
  )

(defn digits_seq
  []
  (->> (iterate inc 0)
       (mapcat digits)
       ))

(defn solution []
  (->> (iterate #(* 10 %) 1)
       (take 7)
       (map #(nth (digits_seq) %))
       (apply *)
       ))

(solution)

;;-> 210
