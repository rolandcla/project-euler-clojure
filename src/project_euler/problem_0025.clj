(ns project-euler.problem_0025)

;; The Fibonacci sequence is defined by the recurrence relation:

;; Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
;; Hence the first 12 terms will be:

;; F1 = 1
;; F2 = 1
;; F3 = 2
;; F4 = 3
;; F5 = 5
;; F6 = 8
;; F7 = 13
;; F8 = 21
;; F9 = 34
;; F10 = 55
;; F11 = 89
;; F12 = 144
;; The 12th term, F12, is the first term to contain three digits.

;; What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

(defn fibonacci-seq
  [a b]
  (lazy-seq (cons a (fibonacci-seq b (+' a b))))
  )

(defn solution []
  (->> (fibonacci-seq 1 1)
       (map-indexed (fn [ix n] [ix n]))
       (drop-while (fn [[ix n]] (< n 1E999M)))
       (ffirst)
       (inc)
       ))

;;-> 4782


