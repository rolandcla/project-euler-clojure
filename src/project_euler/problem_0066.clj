(ns project-euler.problem_0066)

;; Consider quadratic Diophantine equations of the form:

;; x^2 – Dy^2 = 1

;; For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.

;; It can be assumed that there are no solutions in positive integers when D is square.

;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

;; 3^2 – 2×2^2 = 1
;; 2^2 – 3×1^2 = 1
;; 9^2 – 5×4^2 = 1
;; 5^2 – 6×2^2 = 1
;; 8^2 – 7×3^2 = 1

;; Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

;; Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

;; from problem_0064

(defn int-sqrt [x]
  (let [r  (loop [r 1]
             (if (> (* r r) x)
               r
               (recur (* 2 r)) ))]
    (loop [l (quot r 2), r r]
      (if (> (- r l) 1)
        (let [m (/ (+ r l) 2)]
          (if (> (* m m) x)
            (recur l m)
            (recur m r)))
        l
        ))
    ))

;;--------------------------------------------------------
(defn sqr [x] (* x x))

(defn is-square? [x2]
  (let [x (int-sqrt x2)]
    (== x2 (* x x))))

(defn nearest-int-sqrt [x]
  (let [a' (int-sqrt x)]
    (if (< (- x (sqr a')) (- (sqr (+ 1 a')) x)) a' (+ 1 a'))))

(defn diophantine-solution
  "Naive solution !!! Doesn't work :("
  [d x]
  (let [x2-1 (- (* x x) 1)]
    (if (zero? (rem x2-1 d))
      (let [y2 (/ x2-1 d)
            y  (int-sqrt y2)]
        (if (== y2 (* y y))
          y
          nil))
      nil)))

(defn diophantine-min-solution-in-x
  "Naive solution !!! Doesn't work :("
  [d]
  (->> (iterate inc 2)
       (map (fn [x]
              [x  (diophantine-solution d x)]))
       (filter second)
       (first)
       ))

(defn abs [x] (if (< x 0) (- x) x))

(defn mod [x y] (rem (+ y (rem x y)) y))

(defn chakravala
  "From : http://www.isibang.ac.in/~sury/chakravala.pdf"
  [n]
  (let [sqrt-n (int-sqrt n)
        p0  (int-sqrt n)
        q0  1
        m0  (- (sqr p0) n)
        x0  p0]
    (loop [p0 p0, q0 q0, m0 m0, x0 x0]
      (if (== m0 1)
        [p0 q0]
        (let [abs-m0 (abs m0)
              r  (mod (- x0) abs-m0)
              x1 (+ (* (quot (- sqrt-n r) abs-m0) abs-m0) r)
              p1 (/ (+ (* p0 x1) (* n q0)) abs-m0)
              q1 (/ (+ p0 (* x1 q0)) abs-m0)
              m1 (/ (- (sqr x1) n) m0)]
          (recur p1 q1 m1 x1))))

    ))

(defn solution []
  (->> (range 2N 1001)
       (remove is-square?)
       (map (fn [d] (let [[x y] (chakravala d)]
                      [x d])))
       (sort)
       (last)
       (second)
       ))

;;-> 661
