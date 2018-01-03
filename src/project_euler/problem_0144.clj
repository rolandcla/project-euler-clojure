(ns project-euler.problem-0144)

;; In laser physics, a "white cell" is a mirror system that acts as a delay line for the laser beam.
;; The beam enters the cell, bounces around on the mirrors, and eventually works its way back out.

;; The specific white cell we will be considering is an ellipse with the equation 4x^2 + y^2 = 100

;; The section corresponding to −0.01 ≤ x ≤ +0.01 at the top is missing,
;; allowing the light to enter and exit through the hole.

;; The light beam in this problem starts at the point (0.0,10.1) just outside the white cell,
;; and the beam first impacts the mirror at (1.4,-9.6).

;; Each time the laser beam hits the surface of the ellipse,
;; it follows the usual law of reflection "angle of incidence equals angle of reflection."
;; That is, both the incident and reflected beams make the same angle with the normal line at the point of incidence.

;; In the figure on the left,
;; the red line shows the first two points of contact between the laser beam and the wall of the white cell;
;; the blue line shows the line tangent to the ellipse at the point of incidence of the first bounce.

;; The slope m of the tangent line at any point (x,y) of the given ellipse is: m = −4x/y

;; The normal line is perpendicular to this tangent line at the point of incidence.

;; The animation on the right shows the first 10 reflections of the beam.

;; How many times does the beam hit the internal surface of the white cell before exiting?

;;========================================================================================================

(defn scalar-mult [[x y] s] [(* x s) (* y s)])
(defn dot-product [[x0 y0] [x1 y1]] (+ (* x0 x1) (* y0 y1)))
(defn sub-vect    [[x0 y0] [x1 y1]] [(- x0 x1) (- y0 y1)])
(defn reflection [vec-i vec-n]
  (sub-vect (scalar-mult vec-n
                         (* 2 (dot-product vec-n vec-i)))
            vec-i))

(defn distance2 [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn intersection [[x0 y0] [n-x n-y]]
  (let [k (/ n-y n-x)
        j (- y0 (* k x0))
        a (+ 4 (* k k))
        b (* 2 k j)
        c (- (* j j) 100)
        sqrt-d (Math/sqrt (- (* b b) (* 4 a c)))
        x1 (/ (+ b sqrt-d) -2 a)
        x2 (/ (- sqrt-d b) 2 a)]
    (->> [[x1 (+ (* k x1) j)]
          [x2 (+ (* k x2) j)]]
         (apply max-key (fn [p] (distance2 p [x0 y0]))))))

(defn next-hit [[x0 y0] [x1 y1]]
  (let [tan-n (/ y1 x1 -4)
        sec-n (Math/sqrt (+ 1 (* tan-n tan-n)))
        sin-n (/ tan-n sec-n)
        cos-n (/ sec-n)
        vec-n (if (< x1 0) [cos-n (- sin-n)] [(- cos-n) sin-n])
        dx (- x0 x1)
        dy (- y0 y1)
        hyp (Math/sqrt (+ (* dx dx) (* dy dy)))
        vec-i [(/ dx hyp) (/ dy hyp)]
        vec-s (reflection vec-i vec-n)]
    #_(println vec-n vec-i vec-s)
    (intersection [x1 y1] vec-s)))

(defn solution []
  (->> (iterate
        (fn [[p0 p1]]
          [p1 (next-hit p0 p1)])
        [[0.0 10.1] [1.4 -9.6]])
       (take-while (fn [[_ [x y]]] (not (and (> y 9)
                                             (<= -0.01 x 0.01)))))
       count))

;; 354
