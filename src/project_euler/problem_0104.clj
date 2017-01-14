(ns project-euler.problem-0104)

;; The Fibonacci sequence is defined by the recurrence relation:

;; Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
;; It turns out that F541, which contains 113 digits,
;; is the first Fibonacci number for which the last nine digits are 1-9 pandigital
;; (contain all the digits 1 to 9, but not necessarily in order).
;; And F2749, which contains 575 digits,
;; is the first Fibonacci number for which the first nine digits are 1-9 pandigital.

;; Given that Fk is the first Fibonacci number for which the first nine digits
;; AND the last nine digits are 1-9 pandigital, find k.


(defn digits [x]
  (loop [x  x
         ds [(rem x 10)]]
    (let [x' (quot x 10)]
      (if (zero? x')
        ds
        (recur x' (conj ds (rem x' 10))))
      )))

(defn lz-fibo-mod-seq []
  (iterate
   (fn [[f-0 f-1 n]]
     [(rem  (+ f-0 f-1) 1000000000) f-0 (inc n)])
   [1 1 2]
   ))

(def the-digits-1-to-9 (set (range 1 10)))

(def phi (/ (+ (Math/sqrt 5) 1) 2))
(def log-phi (Math/log10 phi))

(def sqrt5 (Math/sqrt 5))
(def log-sqrt5 (Math/log10 sqrt5))

(defn h-fibo [n]
  (let [log-f (- (* log-phi n) log-sqrt5)]
    (->> (if (< log-f 9)
          log-f
          (+ 8 (- log-f (int log-f))))
        (Math/pow 10)
        (int)
        )))

(defn solution []
  (->> (lz-fibo-mod-seq)
       (filter (fn [[f _ n]]
                 (let [ds (digits f)]
                   (and (= the-digits-1-to-9
                           (set ds))
                        (= the-digits-1-to-9
                           (set (digits (h-fibo n))))
                       )
                   )))
       (first)
       (last)
       ))

;;-> 329468

