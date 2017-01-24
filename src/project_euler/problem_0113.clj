(ns project-euler.problem_0113)

;; Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number;
;; for example, 134468.

;; Similarly if no digit is exceeded by the digit to its right it is called a decreasing number;
;; for example, 66420.

;; We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number;
;; for example, 155349.

;; As n increases, the proportion of bouncy numbers below n increases such that
;; there are only 12951 numbers below one-million that are not bouncy
;; and only 277032 non-bouncy numbers below 10^10.

;; How many numbers below a googol (10^100) are not bouncy?


(defn add-one-digit-and-count [last-d [n-inc n-eq n-dec]]
  (->> (range 10)
       (map (fn [d]
              (cond (< d last-d) [0              0    (+ n-dec n-eq)]
                    (> d last-d) [(+ n-inc n-eq) 0    0             ]
                    :else        [n-inc          n-eq n-dec         ]
                    )))
       ))

(defn add-counts [cnts1 cnts2]
  (map (fn [[x1 y1 z1] [x2 y2 z2]] [(+ x1 x2) (+ y1 y2) (+ z1 z2)])
       cnts1
       cnts2))

(def init-counts
  (->> (range 10)
       (map (fn [d] (if (zero? d) [0 0 0] [0 1 0])))
       (vec)
       ))

(def zero-counts
  (->> (range 10)
       (map (fn [d] [0 0 0]))
       (vec)))

(defn update-counts [cnts]
  (reduce add-counts
          zero-counts
          (->> (range 10)
               (map (fn [last-d] (add-one-digit-and-count last-d (nth cnts last-d))))
               )
          ))

(defn solution []
  (->> (iterate update-counts init-counts)
       (take 100)
       (flatten)
       (reduce +)
       ))

;;-> 51161058134250
