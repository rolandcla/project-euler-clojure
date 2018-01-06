(ns project-euler.problem_0146)

;; The smallest positive integer n for which the numbers n^2+1, n^2+3, n2+7, n^2+9, n^2+13, and n^2+27
;; are consecutive primes is 10. The sum of all such integers n below one-million is 1242490.

;; What is the sum of all such integers n below 150 million?

;;=================================================================================================

(defn power2factors [x]
  (loop [s 0 d x]
    (if (odd? d)
      [s d]
      (recur (inc s) (quot d 2)))))

#_(defn prime?
  "Deterministic Miller–Rabin algorithm"
  [n]
  (let [[s d] (power2factors (dec n))
        nn    (biginteger n)]
    (->> [2 3 5 7 11 13 17 19 23] ;; Enough for n < 3.825e18
         (take-while #(< % n))
         (some (fn [a] (let [aa (biginteger a)
                             dd (biginteger d)]
                         #_(println a (.modPow aa dd nn))
                         (and (not= 1 (.modPow aa dd nn))
                              (->> (iterate #(* % 2) d)
                                   (take s)
                                   (every? (fn [r2d]
                                             (not= (dec nn)
                                                   (.modPow aa (biginteger r2d) nn))))))))))))

(defn prime?
  "Deterministic Miller–Rabin algorithm"
  [n]
  (let [[s d] (power2factors (dec n))
        nn    (biginteger n)]
    (->> [2 3 5 7 11 13 17 19 23] ;; Enough for n < 3.825e18
         (take-while #(< % n))
         (every? (fn [a] (let [aa (biginteger a)
                               dd (biginteger d)]
                           (or (== 1 (.modPow aa dd nn))
                               (->> (iterate #(* % 2) d)
                                    (take s)
                                    (some (fn [r2d]
                                            (== (dec nn)
                                                (.modPow aa (biginteger r2d) nn))))))))))))

(defn solution-for [nmax]
  (->> (range 10 nmax 10)
       (remove (fn [n] (or (zero? (mod n 3))
                           (zero? (mod n 7))
                           (zero? (mod n 13)))))
       (filter (fn [n]
                 (let [n2 (* n n)]
                   (->> [1 3 7 9 13 27]
                        (every? (fn [k] (prime? (+ n2 k))))))))
       ))

(def ns<150e6 [10 315410 927070 2525870 8146100 16755190 39313460 97387280 119571820 121288430 130116970 139985660 144774340])

(defn solution []
  (->> ns<150e6
       (filter (fn [n]
                 (== 6
                     (->> (range 1 28 2)
                          (map #(+ % (* n n)))
                          (filter prime?)
                          count))))
       (apply +)))

;; 676333270
