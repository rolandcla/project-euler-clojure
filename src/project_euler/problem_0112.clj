(ns project-euler.problem_0112)

;; Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number;
;; for example, 134468.

;; Similarly if no digit is exceeded by the digit to its right it is called a decreasing number;
;; for example, 66420.

;; We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number;
;; for example, 155349.

;; Clearly there cannot be any bouncy numbers below one-hundred,
;; but just over half of the numbers below one-thousand (525) are bouncy.
;; In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

;; Surprisingly, bouncy numbers become more and more common and by the time we reach 21780
;; the proportion of bouncy numbers is equal to 90%.

;; Find the least number for which the proportion of bouncy numbers is exactly 99%.

(defn bouncy? [x]
  (loop [x x, c 0, d (rem x 10)]
    (if (zero? x)
      false
      (let [d' (rem x 10)
            c' (compare d d')]
        (cond (zero? c') (recur (quot x 10) c d')
              (zero? c)  (recur (quot x 10) c' d')
              (== c c')  (recur (quot x 10) c' d')
              :else      true)
        )
      )))

(defn lz-bouncy-cnt [x c]
  (lazy-seq
   (let [c' (if (bouncy? x)
              (inc c)
              c)]
     (cons [x c'] (lz-bouncy-cnt (inc x) c'))
     )))

(defn solution []
  (->> (lz-bouncy-cnt 1 0)
       (filter (fn [[x c]] (== 99/100 (/ c x))))
       (first)
       (first)
       ))

;;-> 1587000

