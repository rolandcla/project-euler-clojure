(ns project-euler.problem-0140)

;; Consider the infinite polynomial series AG(x) = xG1 + x2G2 + x3G3 + ...,
;; where Gk is the kth term of the second order recurrence relation Gk = Gk−1 + Gk−2, G1 = 1 and G2 = 4;
;; that is, 1, 4, 5, 9, 14, 23, ... .

;; For this problem we shall be concerned with values of x for which AG(x) is a positive integer.

;; The corresponding values of x for the first five natural numbers are shown below.

;; -----------------------
;; |       x	   | AF(x) |
;; |-------------|-------|
;; |  (√5−1)/4   |   1   |
;; |     2/5     |   2   |
;; |  (√22−2)/6  |   3   |
;; | (√137−5)/14 |   4   |
;; |     1/2     |   5   |
;; -----------------------

;; We shall call AG(x) a golden nugget if x is rational, because they become increasingly rarer;
;; for example, the 20th golden nugget is 211345365.

;; Find the sum of the first thirty golden nuggets.

;;===================================================================================================

(def phi (/ (+ 1 (Math/sqrt 5)) 2))

(defn serie [z] (- (/ (- 3 z z) (- 1 (* z z) z)) 3))

(def golden-nuggets
  (->> (iterate inc 2)
       #_(map (fn [d] (let [n (int (/ d phi))]
                        (/ n d))))
       (mapcat (fn [d] (let [n (int (/ d phi))]
                         [(/ (dec n) d) (/ n d)])))
       (map (fn [q] [q (serie q)]))
       (filter (fn [[q af-q]] (and (integer? af-q) (pos? af-q))))
       distinct
       ))

;; After observation of the 1st members:

(defn gen-nd [[n d]] [(+ n d) (+ n d d)])

(defn solution []
  (->> (concat (take 15 (iterate gen-nd [1 2]))
              (take 15 (iterate gen-nd [2 5])))
       (map (fn [[n d]] (serie (/ n d))))
       (apply +)))

;; 5673835352990


