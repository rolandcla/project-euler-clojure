(ns project-euler.problem_0128)

;; A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles,
;; starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise direction.

;; New rings are added in the same fashion,
;; with the next rings being numbered 8 to 19, 20 to 37, 38 to 61,and so on.
;; The diagram below shows the first three rings.

;; By finding the difference between tile n and each of its six neighbours
;; we shall define PD(n) to be the number of those differences which are prime.

;; For example, working clockwise around tile 8 the differences are 12, 29, 11, 6, 1, and 13. So PD(8) = 3.

;; In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10, hence PD(17) = 2.

;; It can be shown that the maximum value of PD(n) is 3.

;; If all of the tiles for which PD(n) = 3 are listed in ascending order to form a sequence,
;; the 10th tile would be 271.

;; Find the 2000th tile in this sequence.

;; From problem 0010 :
;; -------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))
;;------------------------------------------------------------------------

(def mz-prime?
  (memoize prime?))

;;[2, 7 9 19, 7, 2 2 18, 12 12 18, 0 6 18]
;;[2, 7 9 19, 7, 2 2 18, 0 6 12 18]

(defn solution []
  (->> [2, 7 9 19, 7, 2 2 18, 0 6 12 18]
       (iterate
        (fn [[a a1 a2 a3 b b1 b2 b3 i1 i2 i3 i4]]
          [(+ a i2)
           (+ a1 i3) (+ a2 i3) (+ a3 i4)
           (+ b i3)
           (+ b1 i1) (+ b2 i2) (+ b3 i4)
           i2 i3 i4 (+ i4 6)
           ]))
       (mapcat
        (fn [[a a1 a2 a3 b b1 b2 b3 _ _ _ _]]
          [[a (- a1 a) (- a2 a) (- a3 a)]
           [b (- b b1) (- b b2) (- b3 b)]]
          ))
       (filter (fn [[x x1 x2 x3]]
                 (and (mz-prime? x1) (mz-prime? x2) (mz-prime? x3))))
       (take 2000)
       (last)
       (first)
       ))
