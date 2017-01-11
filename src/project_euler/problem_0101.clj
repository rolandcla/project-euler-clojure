(ns project-euler.problem-0101)

;; If we are presented with the first k terms of a sequence
;; it is impossible to say with certainty the value of the next term,
;; as there are infinitely many polynomial functions that can model the sequence.

;; As an example, let us consider the sequence of cube numbers.
;; This is defined by the generating function,
;; un = n^3: 1, 8, 27, 64, 125, 216, ...

;; Suppose we were only given the first two terms of this sequence.
;; Working on the principle that "simple is best" we should assume a linear relationship
;; and predict the next term to be 15 (common difference 7).
;; Even if we were presented with the first three terms, by the same principle of simplicity,
;; a quadratic relationship should be assumed.

;; We shall define OP(k, n) to be the nth term of the optimum polynomial generating function
;; for the first k terms of a sequence.
;; It should be clear that OP(k, n) will accurately generate the terms of the sequence for n ≤ k,
;; and potentially the first incorrect term (FIT) will be OP(k, k+1);
;; in which case we shall call it a bad OP (BOP).

;; As a basis, if we were only given the first term of sequence,
;; it would be most sensible to assume constancy; that is, for n ≥ 2, OP(1, n) = u1.

;; Hence we obtain the following OPs for the cubic sequence:

;;    OP(1, n) = 1	1, 1, 1, 1, ...
;;    OP(2, n) = 7n−6	1, 8, 15, ...
;;    OP(3, n) = 6n2−11n+6     	1, 8, 27, 58, ...
;;    OP(4, n) = n3	1, 8, 27, 64, 125, ...

;; Clearly no BOPs exist for k ≥ 4.

;; By considering the sum of FITs generated by the BOPs (indicated in red above),
;; we obtain 1 + 15 + 58 = 74.

;; Consider the following tenth degree polynomial generating function:

;; un = 1 − n + n^2 − n^3 + n^4 − n^5 + n^6 − n^7 + n^8 − n^9 + n^10

;; Find the sum of FITs for the BOPs.

(defn poly-mult [xs xj]
  (vec (map +
            (conj xs 0)
            (map #(* % (- xj)) (cons 0 xs))
            )))

(defn lagrange-basis [xs j]
  (let [xj (nth xs j)]
    (->> xs
         (map-indexed (fn [m xm] [m xm]))
         (remove (fn [[m _]] (== m j)))
         (reduce (fn [[ps denom] [_  xm]]
                   [(poly-mult ps xm) (* denom (- xj xm))])
                 [[1] 1])
         ((fn [[ps denom]]
            (map #(/ % denom) ps)))
         )))

(defn lagrange-interpolation [xs ys]
  (->> ys
       (map-indexed (fn [j y]
                      (map #(* y %) (lagrange-basis xs j))))
       (reduce #(map + %1 %2))
       ))