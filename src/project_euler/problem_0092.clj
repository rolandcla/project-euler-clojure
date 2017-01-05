(ns project-euler.problem_0092)

;; A number chain is created by continuously adding the square of the digits in a number to form a new number
;; until it has been seen before.

;; For example,

;; 44 → 32 → 13 → 10 → 1 → 1
;; 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

;; Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
;; What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

;; How many starting numbers below ten million will arrive at 89?

(defn digits [x]
  (loop [x x, ds nil]
    (cond (zero? x) ds
          :else     (recur (quot x 10) (conj ds (rem x 10))))))

(defn solution-for [m]
  (->> (range 1 m)
       (reduce (fn [[set-89 set-1 cnt-89] n]
                 (loop [n' n, xs []]
                   (let [x (apply + (map #(* % %) (digits n')))]
                     (cond (set-89 x) [(apply conj set-89 xs) set-1 (inc cnt-89)]
                           (set-1 x)  [set-89 (apply conj set-1 xs) cnt-89]
                           :else      (recur x (conj xs x)))
                     ))
                 )
               [#{89} #{1} 0])
       (last)
       ))

(defn solution [] (solution-for 10000000))

;;-> 8581146
