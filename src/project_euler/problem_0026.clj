(ns project-euler.problem_0026)

;; A unit fraction contains 1 in the numerator.
;; The decimal representation of the unit fractions with denominators 2 to 10 are given:

;; 1/2	= 	0.5
;; 1/3	= 	0.(3)
;; 1/4	= 	0.25
;; 1/5	= 	0.2
;; 1/6	= 	0.1(6)
;; 1/7	= 	0.(142857)
;; 1/8	= 	0.125
;; 1/9	= 	0.(1)
;; 1/10	= 	0.1
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
;; It can be seen that 1/7 has a 6-digit recurring cycle.

;; Find the value of d < 1000 for which 1/d contains
;; the longest recurring cycle in its decimal fraction part.

;;==========================================================================

;; Naive solution
;;--------------------------------------------------------------------------

(defn decimals
  [x]
  {:pre [(integer? x)(> x 1)]}

  (letfn [(recur-dec [n]
            (let [q (quot n x), r (rem n x)]
              (if (== 0 r)
                (if (== 0 q) [] [q])
                (lazy-seq (cons q (recur-dec (* 10 r))))
                )
              ))]
    (recur-dec 10)
    )
  )

(defn search-for-n-digits-cycle
  [decs n]
  (let [n-cy 4
        farthest-cy 20
        ]
    (loop [from 0]
      (when (< from farthest-cy)
        (let [p (partition n (take (* n-cy n) (drop from decs)))]
          (if (= (count p) n-cy)
            (if (apply = p)
              [(take from decs) (first p)]
              (recur (inc from)))
            [decs nil]
            ))))))




(defn search-for-shortest-cycle
  [ds]
  (loop [n 1]
    (or (search-for-n-digits-cycle ds n)
        (recur (inc n)))
    )
  )

(defn naive-solution [n]
  (->> (range 2 n)
       (map #(vector % (decimals %)))
       (map (fn [[ix ds]] [ix (search-for-shortest-cycle ds)]))
       (map (fn [[ix [prefix cy]]] [ix (count cy)]))
       (sort-by (fn [[ix n]] n))
       ))

;; Best solution - 7000 times faster :)
;; -------------------------------------------------------------

(defn length-of-recurring-cycle
  [n]
  (loop [ix 0, rest 10, seen {}]
    (let [prev-rest-ix (seen rest)]
      (cond (== 0 rest)          0
            (nil? prev-rest-ix)  (recur (inc ix) (* 10 (rem rest n)) (assoc seen rest ix))
            :else                (- ix prev-rest-ix)
            ))))

(defn longest-recurring-cycle [n]
  (->> (range 2 n)
       (map #(vector (length-of-recurring-cycle %) %))
       (reduce #(if (> (first %2) (first %1)) %2 %1))
       ))

(defn solution []
  (second (longest-recurring-cycle 1000)))

;;-> [982 983]
;;        ---

