(ns project-euler.problem_0148)


;; We can easily verify that none of the entries in the first seven rows of Pascal's triangle are divisible by 7:

;;  	 	 	 	 	 	 1
;;  	 	 	 	 	 1	 	 1
;;  	 	 	 	 1	 	 2	 	 1
;;  	 	 	 1	 	 3	 	 3	 	 1
;;  	 	 1	 	 4	 	 6	 	 4	 	 1
;;  	 1	 	 5	 	10	 	10	 	 5	 	 1
;; 1	 	 6	 	15	 	20	 	15	 	 6	 	 1
;; However, if we check the first one hundred rows,
;; we will find that only 2361 of the 5050 entries are not divisible by 7.

;; Find the number of entries which are not divisible by 7 in the first one billion (10^9) rows of Pascal's triangle.

(defn rows []
  (iterate (fn [row]
             (let [r (conj row 0)]
               (map (fn [x y] (+' x y)) r (reverse r))))
           [1]))

(defn explore [n]
  (->> (rows)
       (take n)
       (map (fn [row] (->> (filter #(not= 0 (mod % 7)) row)
                           count)))
       #_(apply +)))

(defn sim-cnt []
  (for [w (range 1 8)
        x (range 1 8)
        y (range 1 8)
        z (range 1 8)]
    (* w x y z)))

(defn explore-sim [n]
  (->> (sim-cnt)
       (take n)
       (apply +)))

;; By observation

(defn base7-digits [x]
  (loop [x x ds '()]
    (if (zero? x)
      ds
      (recur (quot x 7) (conj ds (mod x 7)))
      )))


;; 0 1  1
;; 1 2  3
;; 2 3  6
;; 3 4 10
;; 4 5 15
;; 5 6 21
;; 6 7 28

(def trg [1 3 6 10 15 21 28])
(def trg [0 1 3 6 10 15 21])

;; (defn solution-for [n]
;;   (let [d7s (base7-digits n)
;;         sol (explore-sim n)]
;;     [ sol
;;      (case (count d7s)
;;        1 (trg (nth d7s 0))
;;        2 (+ (* 28 (trg (nth d7s 0))) (* (inc (nth d7s 0)) (trg (nth d7s 1))))
;;        3 (+ (* 28 28 (trg (nth d7s 0)))
;;             (* 28 (inc (nth d7s 0)) (trg (nth d7s 1)))
;;             (* (inc (nth d7s 0))  (inc (nth d7s 1)) (trg (nth d7s 2))))
;;        )]))

(defn solution-for [n]
  (let [d7s (base7-digits n)
        sol (explore-sim n)]
    (->> (range (count d7s))
         (map (fn [i]
                (->> (range (count d7s))
                     (map (fn [j]
                            (cond (< j i) (inc (nth d7s j))
                                  (> j i) 28
                                  :else   (trg (nth d7s j)))))
                     (apply *))))
         (apply +))))

(defn solution []
  (solution-for 1000000000))
