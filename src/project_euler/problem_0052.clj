(ns project-euler.problem_0052)

;; It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
;; but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

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

(defn solution[]
  (->> (iterate inc 1)
       (some (fn [x]
               (when (apply = (map #(set (digits (* x %))) [1 2 3 4 5 6]))
                 x)
               ))
   ))

::-> 142857
