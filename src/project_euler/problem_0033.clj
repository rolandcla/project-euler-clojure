(ns project-euler.problem_0033)

;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician
;; in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct,
;; is obtained by cancelling the 9s.

;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

;; There are exactly four non-trivial examples of this type of fraction,
;; less than one in value, and containing two digits in the numerator and denominator.

;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.

(defn the-2-digits
  [n]
  [(quot n 10) (rem n 10)]
  )

(defn solution []
  (->> (for [n (range 11 100) d (range (inc n) 100)] [n d])
       (filter (fn [[n d]]                    (not (== 0 (rem n 11)))))
       (map    (fn [[n d]]                    [[n d] (the-2-digits n) (the-2-digits d)]))
       (filter (fn [[[n d] [n1 n0] [d1 d0]]]  (not (== 0 (* n0 d0)))))
       (mapcat (fn [[[n d] [n1 n0] [d1 d0]]]  (concat (when (== n0 d1) [[[n d] (/ n1 d0)]])
                                                      (when (== n1 d0) [[[n d] (/ n0 d1)]]))))
       (filter (fn [[[n d] qd]]               (== (/ n d) qd)))
       (map    (fn [[[n d] qd]]               qd))
       (reduce *)
       (denominator)
       ))

(solution)

;;-> 100


