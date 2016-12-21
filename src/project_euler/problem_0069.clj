(ns project-euler.problem_0069)

;; Euler's Totient function, φ(n) [sometimes called the phi function],
;; is used to determine the number of numbers less than n which are relatively prime to n.
;; For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.

;; n	 | Relatively Prime	| φ(n) | n/φ(n)
;; ----|------------------|------|--------------
;; 2	 | 1	              | 1	   | 2
;; 3	 | 1,2	            | 2	   | 1.5
;; 4	 | 1,3	            | 2	   | 2
;; 5	 | 1,2,3,4	        | 4	   | 1.25
;; 6	 | 1,5	            | 2	   | 3
;; 7	 | 1,2,3,4,5,6     	| 6	   | 1.1666...
;; 8	 | 1,3,5,7	        | 4	   | 2
;; 9	 | 1,2,4,5,7,8    	| 6	   | 1.5
;; 10  |	1,3,7,9	        | 4	   | 2.5
;; -------------------------------------------

;; It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

;; Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

;; Based on problem 0003: prime-factors
;; -----------------------------------------------

(defn totient
  [n]
  (letfn
   [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [n n, m 2, t 1, prev-m 1]
      (let [mmax (+ (int (Math/sqrt n)) 1)]
        (cond (== 1 n)         t
              (> m mmax)       (* t (if (== n prev-m) n (- n 1)))
              (== 0 (rem n m)) (recur (/ n m) m (* t (if (== m prev-m) m (- m 1))) m)
              :else            (recur n (next-fact m) t prev-m) )))))

(defn solution []
  (loop [n 2 r-max 0 n-max nil]
    (if (> n 1000000)
      n-max
      (let [r (/ n (totient n))]
        (if (> r r-max)
          (recur (inc n) r n)
          (recur (inc n) r-max n-max))))))

;;-> 510510
