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

(defn lz-digits [x]
  (lazy-seq
   (let [d  (rem x 10)
         x' (quot x 10)]
     (if (zero? x')
       [d]
       (cons d (lz-digits x'))))))

(defn lz-fibo-seq []
  (iterate
   (fn [[f-0 f-1 n]]
     [(+' f-0 f-1) f-0 (inc n)])
   [1 1 2]
   ))

(def the-digits-1-to-9 (set (range 1 10)))

(def phi (/ (+ (Math/sqrt 5) 1) 2))
(def log-phi (Math/log10 phi))

(def sqrt5 (Math/sqrt 5))
(def log-sqrt5 (Math/log10 sqrt5))

(defn solution []
  (->> (iterate inc 1)
       (map (fn [n]
                 (-> (- (* log-phi n) log-sqrt5)
                     (#(+ 10 (- % (int %))))
                     (Math/pow 10)
                     )))
       (take 10)
       ))


;; (defn solution []
;;   (->> (lz-fibo-seq)
;;        (filter (fn [[f _ n]]
;;                  (let [ds (lz-digits f)]
;;                    (and (= the-digits-1-to-9
;;                            (set (take 9 ds)))
;;                         (= the-digits-1-to-9
;;                            (set (take-last 9 ds))))
;;                    )))
;;        (first)
;;        (last)
;;        ))
