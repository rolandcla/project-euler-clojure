(ns project-euler.problem_0135)

;; Given the positive integers, x, y, and z, are consecutive terms of an arithmetic progression,
;; the least value of the positive integer, n, for which the equation, x^2 − y^2 − z^2 = n,
;; has exactly two solutions is n = 27:
;; 34^2 − 27^2 − 20^2 = 12^2 − 9^2 − 6^2 = 27
;;
;; It turns out that n = 1155 is the least value which has exactly ten solutions.
;;
;; How many values of n less than one million have exactly ten distinct solutions?

;;----------------------------------------------------------------------------------------------

;; Let d be the increment of the arithmetic progression
;; The equation becomes: n = (z + 2d)^2 - (z + d)^2 - z^2
;; -> n = z^2 + 4dz + 4d^2 - z^2 - 2dz - d^2 - z^2
;; -> n = -z^2 + 2dz + 3d^2

;; Solutions for n = 0:
;; D = 4d^2 + 12d^2 = 16d^2
;; z1, z2 = (-2d +- 4d) / -2
;; z1, z2 = 3d, -d

;; Solutions for n = 1000000
;; D = 4d^2 + 4(3d^2 - 1000000) = 16d^2 - 4e6
;; z1, z2 = (-2 +- sqrt(D)) / -2

(defn equation [z d] (+ (* -1 z z) (* 2 d z) (* 3 d d)))

(defn sols-for-1e6 [d]
  (let [delta (- (* 16 d d) 4e6)
        sqrt_d (Math/sqrt delta)]
    [(/ (+ (* -2 d) sqrt_d) -2)
     (/ (- (* -2 d) sqrt_d) -2)]))

(defn z-range [d]
  (let [[z0 z1] (sols-for-1e6 d)]
    (if (Double/isNaN z0)
      (range 1 (* 3 d))
      (concat (range 1 z0) (range (inc (int z1)) (* 3 d)))
      )))

(defn solution []
  (->> (iterate inc 1)
       (map (fn [d] [d (z-range d)]))
       (take-while (fn [[_ zs]] (seq zs)))
       (mapcat (fn [[d zs]] (map (fn [z] (equation z d)) zs)))
       frequencies
       (filter (fn [[_ cnt]] (== cnt 10)))
       count
       ))

;;-> 4989
