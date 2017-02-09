(ns project-euler.problem_0129
  (:require [clojure.math.combinatorics :as combo]))

;; A number consisting entirely of ones is called a repunit.
;; We shall define R(k) to be a repunit of length k; for example, R(6) = 111111.

;; Given that n is a positive integer and GCD(n, 10) = 1,
;; it can be shown that there always exists a value, k, for which R(k) is divisible by n,
;; and let A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.

;; The least value of n for which A(n) first exceeds ten is 17.

;; Find the least value of n for which A(n) first exceeds one-million.

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

(defn distinct-prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs #{}]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))

(defn totient [x]
  (reduce (fn [prod p] (* prod (- 1 (/ p))))
          x
          (distinct-prime-factors x)))

(defn pow [x n]
  (loop [p 1 x x n n]
    (if (zero? n)
      p
      (recur (if (odd? n)
               (*' p x)
               p)
             (*' x x)
             (quot n 2)
             ))))

(defn k [n]
  (totient (* 9 n)))

(defn repunit [n]
  (/ (- (pow 10N n) 1) 9))

(defn solution-for [n]
  (->> (iterate inc (inc n))
       (filter odd?)
       (filter prime?)
       ;;(take 50)
       (map (fn [p]
              [p
               (->> (prime-factors (dec p))
                    (combo/subsets)
                    (next)
                    (butlast)
                    (map (fn [pf-ss] (reduce * pf-ss)))
                    (some (fn [div] (zero? (rem (repunit div) p))))
                    )]))
       (filter (fn [[p div-ok]] (not div-ok)))
       (first)
       ))

(defn solution []
  (->> (iterate inc 1)
       (map (fn [n] [n (k n)]))
       (filter (fn [[n k]] (> k 1000000)))
       (first)
       ))
