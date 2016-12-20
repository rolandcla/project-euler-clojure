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

(defn diophantine-solution [d x]
  (let [x2-1 (- (* x x) 1)]
    (if (zero? (rem x2-1 d))
      (let [y2 (/ x2-1 d)
            y  (int-sqrt y2)]
        (if (== y2 (* y y))
          y
          nil))
      nil)))

(defn diophantine-min-solution-in-x [d]
  (->> (iterate inc 2)
       (map (fn [x]
              [x  (diophantine-solution d x)]))
       (filter second)
       (first)
       ))

(defn chakravala [d]
  (let [a  (nearest-int-sqrt d)
        b  1
        k  (- (sqr a) d)]
    (println "a b k : " a b k)

    ))

(defn solution []
  (->> (range 2 1001)
       (remove is-square?)
       (map (fn [d] (let [[x y] (diophantine-min-solution-in-x d)]
                      [x d])))
       (sort)
       (last)
       (second)
       ))
