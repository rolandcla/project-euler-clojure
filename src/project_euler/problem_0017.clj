(ns project-euler.problem_0017)

;; If the numbers 1 to 5 are written out in words: one, two, three, four, five,
;; then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
;; how many letters would be used?


;; NOTE: Do not count spaces or hyphens.
;; For example, 342 (three hundred and forty-two) contains 23 letters
;; and 115 (one hundred and fifteen) contains 20 letters.
;; The use of "and" when writing out numbers is in compliance with British usage.

(defn number-of-letters
  [n]
  {:pre [(<= 0 n 1000)]}

  (cond (== n 1000) 11
        (>= n 100) (+  (number-of-letters (int (/ n 100)))
                       7
                       (if (== 0 (rem n 100))
                         0
                         (+ 3  (number-of-letters (rem n 100)))))
        (>= n 20) (+  ({2 6, 3 6, 4 5, 5 5, 6 5, 7 7, 8 6, 9 6} (int (/ n 10)))
                      (number-of-letters (rem n 10)))
        :else  ([0 3 3 5 4 4 3 5 5 4 3 6 6 8 8 7 7 9 8 8] n)
        )
  )

(defn solution []
  (reduce + (map number-of-letters (range 1 1001))))

;;-> 21124

