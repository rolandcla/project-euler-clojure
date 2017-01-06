(ns project-euler.problem_0095)

;; The proper divisors of a number are all the divisors excluding the number itself.
;; For example, the proper divisors of 28 are 1, 2, 4, 7, and 14.
;; As the sum of these divisors is equal to 28, we call it a perfect number.

;; Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220,
;; forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.

;; Perhaps less well known are longer chains.
;; For example, starting with 12496, we form a chain of five numbers:

;; 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

;; Since this chain returns to its starting point, it is called an amicable chain.

;; Find the smallest member of the longest amicable chain with no element exceeding one million.


;; from problem 0012 ....................................................................................
(defn factors
  [x]
  (->> (range 1 (inc (int (Math/sqrt x))))
       (filter #(== 0 (rem x %)))
       (mapcat #(let [q (/ x %)]
                  (if (== q %) [%] [% q])))
       ))
;;........................................................................................................

;; from problem 0021 ....................................................................................
(defn sum-of-proper-divisors
  [n]
  (->> (factors n)
       (filter #(not (== n %)))
       (reduce +)
       ))
;;........................................................................................................

(defn search-chain [n]
  (loop [n n, seen-pos {}, seen-vec [], cnt 0]
    (if (> n 1000000)
      [seen-vec (count seen-vec)]
      (if-let [ix (seen-pos n)]
        [seen-vec ix]
        (recur (sum-of-proper-divisors n) (assoc seen-pos n cnt) (conj seen-vec n) (inc cnt))))
    ))

(defn solution []
  (->> (loop [not-seen (set (range 1000000))
              l-chain  []]
         (if (seq not-seen)
           (let [[seen ch-ix] (search-chain (first not-seen))
                 ch           (subvec seen ch-ix)]
             (if (> (count ch) (count l-chain))
               (recur (apply disj not-seen seen) ch)
               (recur (apply disj not-seen seen) l-chain)))
           l-chain)
         )))
