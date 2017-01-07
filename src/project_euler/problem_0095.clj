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


(defn search-longuest-chain-from [m]
  (loop [seen    (apply vector-of :int (repeat m -1))
         i       0
         l-chain []]
    (if (< i m)
      (if (== -1 (seen i))
        (let [[ch ch-loop] (loop [x  i
                                ch [x]]
                           (let [y (sum-of-proper-divisors x)]
                             (if (and (< y m) (== -1 (seen y)))
                               (let [ix (.indexOf ch y)]
                                 (if (>= ix 0)
                                   [(conj ch y) (subvec ch ix)]
                                   (recur y (conj ch y))
                                   ))
                               [ch []]
                               )
                             ))]
          ;;(println i ch ch-loop)
          (let [seen' (reduce (fn [s v] (assoc s v 0)) seen ch)
                l-chain' (if (> (count ch-loop) (count l-chain)) ch-loop l-chain)]
            (recur seen' (inc i) l-chain')))
        (recur seen (inc i) l-chain))
      l-chain)
    ))

(defn solution []
  (->> (search-longuest-chain-from 1000000)
       (sort)
       (first)
       ))

;;-> 14316

