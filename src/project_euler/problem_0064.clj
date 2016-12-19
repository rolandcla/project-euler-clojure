(ns project-euler.problem_0064)


;; All square roots are periodic when written as continued fractions and can be written in the form:

;;            1
;; √N = a0 +	----------
;;                 1
;;           a1 + ---------
;;                      1
;;  	 	           a2 +	---------
;;                	 	 	a3 + ...
;; For example, let us consider √23:

;;                              1                   1
;; √23 = 4 + √23 — 4 = 4 + -----------  = 4 +  ------------	
;;                              1                  √23 – 3
;;                          ---------          1 + -------
;;                            √23—4                   7
;;
;; If we continue we would get the following expansion:
;;            1
;; √23 = 4 + ----------
;;                1
;;           1 + ---------
;;                    1
;;               3 + ---------
;;                        1
;;                   1 + ---------
;;                       8 + ......

;; The process can be summarised as follows:

;; It can be seen that the sequence is repeating.
;; For conciseness, we use the notation √23 = [4;(1,3,1,8)],
;; to indicate that the block (1,3,1,8) repeats indefinitely.

;; The first ten continued fraction representations of (irrational) square roots are:

;; √2=[1;(2)], period=1
;; √3=[1;(1,2)], period=2
;; √5=[2;(4)], period=1
;; √6=[2;(2,4)], period=2
;; √7=[2;(1,1,1,4)], period=4
;; √8=[2;(1,4)], period=2
;; √10=[3;(6)], period=1
;; √11=[3;(3,6)], period=2
;; √12= [3;(2,6)], period=2
;; √13=[3;(1,1,1,1,6)], period=5

;; Exactly four continued fractions, for N ≤ 13, have an odd period.

;; How many continued fractions for N ≤ 10000 have an odd period?

(defn square [x] (* x x))

(defn int-sqrt [x]
  (let [r  (loop [r 1]
             (if (> (* r r) x)
               r
               (recur (* 2 r)) ))]
    (loop [l (quot r 2), r r]
      (if (> (- r l) 1)
        (let [m (/ (+ r l) 2)]
          (if (> (* m m) x)
            (recur l m)
            (recur m r)))
        l
        ))
    ))

(defn pgcd [a b]
  (let [r (rem a b)]
    (if (zero? r)
      b
      (recur b r))))

(defn seq-cont-fract [x]
  (let [sqrt-x (int-sqrt x)]
    (if (== x (* sqrt-x sqrt-x))
      [[sqrt-x nil nil]]
      (iterate (fn [[c n d]]
                 (let [d' (/ (- x (* d d)) n)
                       c' (quot (+ sqrt-x d) d')
                       n' (- (* d' c') d)]
                   [c' d' n']))
               [sqrt-x 1 sqrt-x]))
    ))


(defn search-cycle [x]
  (loop [[[c n d] & rest-seq] (seq-cont-fract x)
         seen-set #{}
         cs []]
    (if (seq rest-seq)
      (if (get seen-set [n d])
        (conj cs c)
        (recur rest-seq (conj seen-set [n d]) (conj cs c)))
      [c])))

(defn solution []
  (->> (range 10001)
       (map search-cycle)
       (filter #(and (> (count %) 1) (even? (count %))))
       (count)
       ))

;;-> 1322
