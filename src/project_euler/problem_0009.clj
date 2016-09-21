(ns project-euler.problem_0009)

;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

;; a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.


(defn pythagorean-triplet
  [s]
  (->> (for [a (range 1       (+ 1 (int (/ s 3))))
             b (range (+ 1 a) (+ 1 (int (/ s 2))))
             c (range (+ 1 b) s)]
         [a b c])
       (filter (fn [[a b c]] (== (+ (* a a) (* b b)) (* c c))))
       (filter #(== s (apply + %)))
       ))

(defn solution []
  (apply * (first (pythagorean-triplet 1000))))

;;-> 31875000


