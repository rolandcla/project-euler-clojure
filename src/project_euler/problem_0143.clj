(ns project-euler.problem_0143)

;; Let ABC be a triangle with all interior angles being less than 120 degrees.
;; Let X be any point inside the triangle and let XA = p, XC = q, and XB = r.

;; Fermat challenged Torricelli to find the position of X such that p + q + r was minimised.

;; Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC
;; are constructed on each side of triangle ABC, the circumscribed circles of AOB, BNC, and AMC
;; will intersect at a single point, T, inside the triangle.
;; Moreover he proved that T, called the Torricelli/Fermat point, minimises p + q + r.
;; Even more remarkable, it can be shown that when the sum is minimised,
;; AN = BM = CO = p + q + r and that AN, BM and CO also intersect at T.

;; If the sum is minimised and a, b, c, p, q and r are all positive integers
;; we shall call triangle ABC a Torricelli triangle.
;; For example, a = 399, b = 455, c = 511 is an example of a Torricelli triangle, with p + q + r = 784.

;; Find the sum of all distinct values of p + q + r ≤ 120000 for Torricelli triangles.

(defn int-triangle-120 [max-perim]
  (for [z (range 1 (inc (int (/ max-perim (+ 2 (Math/sqrt 3))))))
        :let [z2 (* z z)
              max-xy (- max-perim z)]
        y (iterate inc z)
        :let [x2 (+ (* y y) z2 (* y z))
              x (int (Math/sqrt x2))]
        :while (<= (+ x y) max-xy)
        :when (== x2 (* x x))
        ]
    [x y z]))

(defn solution-for [max-pqr]
  (let [all-trg120 (int-triangle-120 (* 2 max-pqr))
        z->y (reduce (fn [m [x y z]] (-> m (update y conj z) (update z conj y))) {} all-trg120)
        yz-set (reduce (fn [s [x y z]] (conj s #{y z})) #{} all-trg120)]
    (->> (for [[b p q] all-trg120
               r (z->y p)
               :when (and (not= q r) (yz-set #{q r}))]
           (+ p q r))
         (filter #(<= % max-pqr))
         distinct
         (apply +))))

;; OK mais très lent, il faudrait une meilleure méthode int-triangle-120 (voir Eisenstein triple)
(defn solution [] (solution-for 120000))

;; 30758397
