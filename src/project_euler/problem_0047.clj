(ns project-euler.problem_0047)

;; The first two consecutive numbers to have two distinct prime factors are:

;; 14 = 2 × 7
;; 15 = 3 × 5

;; The first three consecutive numbers to have three distinct prime factors are:

;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.

;;Find the first four consecutive integers to have four distinct prime factors each.
;; What is the first of these numbers?

(defn n-distinct-prime-factors?
  [n x]
  {:pre [(integer? x) (> x 0)]}

  (letfn
      [(next-fact [x] (if (== x 2) 3 (+ x 2)))]
    (loop [x x, m 2, prev_m 1, c 0]
      (let [mmax (+ (int (Math/sqrt x)) 1)]
        ;;(println x m prev_m c mmax)
        (cond (> c n)          false
              (> m mmax)       (== n (if (== x prev_m) c (inc c)))
              (== 0 (rem x m)) (recur (/ x m) m m (if (== m prev_m) c (inc c)))
              :else            (recur x (next-fact m) prev_m c)
              )))))

(defn solution []
  (loop [x 1, t1 false, t2 false, t3 false]
    (let [t (n-distinct-prime-factors? 4 x)]
      (if (and t t1 t2 t3)
        (- x 3)
        (recur (inc x) t t1 t2)
        ))))

(solution)
;;-> 134043
