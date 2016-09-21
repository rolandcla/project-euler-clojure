(ns project-euler.problem_0003)

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?


(defn prime-factors
  [n]
  {:pre [(integer? n) (> n 0)]} 

  (letfn
   [(next-fact [n] (if (== n 2) 3 (+ n 2)))]

   (loop [n n, m 2, factors []]
     (let [mmax (+ (int (Math/sqrt n)) 1)]
       (cond (> m mmax)       (conj factors n)
             (== 0 (rem n m)) (recur (/ n m) m (conj factors m))
             :else            (recur n (next-fact m) factors) )))))

(defn solution []
  (last (prime-factors 600851475143)))

;;-> 6857




