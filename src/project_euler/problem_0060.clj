(ns project-euler.problem_0060)

;; The primes 3, 7, 109, and 673, are quite remarkable.
;; By taking any two primes and concatenating them in any order the result will always be prime.
;; For example, taking 7 and 109, both 7109 and 1097 are prime.
;; The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;----------------------------------------------------------------------------

;; #{194119 3 5923 67 37} is a solution, but not those with the lowest sum...

(defn concat-numbers [x y]
  (loop [x x, q y]
    (if (zero? q)
      (+' x y)
      (recur (*' x 10) (quot q 10)))))

(defn concats-are-primes? [x y]
  (and (prime? (concat-numbers x y)) (prime? (concat-numbers y x))))


(defn solutions-from [init-seq p-set]
  (let [npc-seq (->> (for [xs init-seq, y p-set] [xs y])
                     (filter (fn [[xs y]] (every? (partial concats-are-primes? y) xs)))
                     (map (fn [[xs y]] (sort (conj xs y))))
                     (set)
                     )]
    (println (count npc-seq))
    npc-seq
    ))

(defn solutions [maxs]
  (reduce solutions-from
          (map (fn [p] [p]) (filter prime? (range 2 (first maxs))))
          (map #(filter prime? (range 2 %)) (next maxs))
          ))


;; #{194119 3 5923 67 37} is a solution, but not those with the lowest sum...
;; sum == 76721 is a solution but not the best...
;; (467 941 2099 19793 25253) (sum == 48553) ok..... but not the best
;; (7 1237 2341 12409 18433) (sum == 34427) ok..... grrrr.....
;; (13 5197 5701 6733 8389) (sum == 26033) :)
(defn solution [] (->> (solutions [5300 7000 9000 13020 26033])
                       (map #(apply + %))
                       ;;(apply min)
                       ))

;;-> 26033

