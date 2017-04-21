(ns project-euler.problem_0133)

;; A number consisting entirely of ones is called a repunit.
;; We shall define R(k) to be a repunit of length k; for example, R(6) = 111111.

;; Let us consider repunits of the form R(10n).

;; Although R(10), R(100), or R(1000) are not divisible by 17, R(10000) is divisible by 17.
;; Yet there is no value of n for which R(10n) will divide by 19.
;; In fact, it is remarkable that 11, 17, 41, and 73 are the only four primes below one-hundred
;; that can be a factor of R(10n).

;; Find the sum of all the primes below one-hundred thousand that will never be a factor of R(10n).

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

(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))

(defn a [n]
  (loop [r 1 k 1]
    (let [r' (rem r n)]
      (if (zero? r')
        k
        (recur (+ 1 (* 10 r')) (inc k))
        ))))

(defn solution []
  (->> (concat [3] (range 7 100000 2))
       (filter prime?)
       (map (fn [n] [n (a n)]))
       (remove #(every? #{2 5} (prime-factors (second %))))
       (map first)
       (apply + 2 5)
       ))

;;-> 453647705
