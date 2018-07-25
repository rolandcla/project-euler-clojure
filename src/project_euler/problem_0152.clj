(ns project-euler.problem-0152
  (:require [clojure.math.combinatorics :as combo]))

;; There are several ways to write the number 1/2 as a sum of inverse squares using distinct integers.

;; For instance, the numbers {2,3,4,5,7,12,15,20,28,35} can be used:

;; In fact, only using integers between 2 and 45 inclusive,
;; there are exactly three ways to do it, the remaining two being:
;; {2,3,4,6,7,9,10,20,28,35,36,45} and {2,3,4,6,7,9,12,15,28,30,35,36,45}.

;; How many ways are there to write the number 1/2 as a sum of inverse squares
;; using distinct integers between 2 and 80 inclusive?

;;(def inv-squares (vec (for [x (range 2 36)] (/ 1 x x))))

(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))


(def max-range 80)
;;(def max-range 45)

(def primes [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79])

(defn sum-of-inv-sq [xs]
  (reduce (fn [s x] (+ s (/ 1 x x))) 0 xs))

(defn simplifiable-combs [prime]
  (->> (range prime (inc max-range) prime)
       (combo/subsets)
       (remove empty?)
       (filter (fn [xs] (not= 0
                              (rem (denominator (sum-of-inv-sq xs))
                                   prime))))))

(def numbers
  (as-> (range 2 (inc max-range)) _*_
    (remove (fn [x] (>= (last (prime-factors x)) 7)) _*_)
    (into _*_ (flatten (simplifiable-combs 7)))
    (into _*_ (flatten (simplifiable-combs 13)))
    (set _*_)
    (sort _*_)
     ))

(def squares (vec (for [x numbers] (* x x))))
(def i-squares (vec (for [x squares] (/ x))))
(def r-sum (reduce (fn [ss x] (conj ss (+ x (or (first ss) 0)))) '() (reverse i-squares)))

(def cache-lim 37)
(def cache
  (->> numbers
       (drop-while #(< % cache-lim))
       (combo/subsets)
       (group-by sum-of-inv-sq)
       ))

(defn explore
  [nir s nn]
  (when-let [[n i r] (first nir)]
    (when (>= r s)
      (cond (== i s)  (concat [(conj nn n)] (explore (rest nir) s nn))
            (> n cache-lim) (when-let [cs (cache s)]
                        (map #(into nn %) cs))
            :else     (concat (explore (rest nir) (- s i) (conj nn n))
                              (explore (rest nir) s nn))))))

(defn solution
  []
  (-> (explore (map vector numbers i-squares r-sum)
               1/2
               [])
      count))
