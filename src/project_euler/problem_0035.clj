(ns project-euler.problem_0035)

;; The number, 197, is called a circular prime because all rotations of the digits:
;; 197, 971, and 719, are themselves prime.

;; There are thirteen such primes below 100:
;; 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;; How many circular primes are there below one million?

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;....................................................................

(defn higher-10-pow
  [n]
  (loop [n n, p 10]
    (let [q (quot n 10)]
      (if (== 0 q)
        p
        (recur q (* p 10))))))

(defn rotations
  [n]
  (let [p (higher-10-pow n)]
    (for [d (iterate #(* % 10) 1)
          :while (< d p)]
      (+ (quot n d) (* (/ p d) (rem n d)))
      )
    )
  )

(defn solution []
  (->> (cons 2 (range 3 1000000))
       (reduce (fn [circ-prime-set n]
                 (if (get circ-prime-set n)
                   circ-prime-set
                   (let [ns (rotations n)]
                     (if (every? prime? ns)
                       (apply conj circ-prime-set ns)
                       circ-prime-set))))
               #{})
       (count)
       ))

(solution)

;;-> 55

