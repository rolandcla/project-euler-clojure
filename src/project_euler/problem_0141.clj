(ns project-euler.problem_0141)

;; A positive integer, n, is divided by d and the quotient and remainder are q and r respectively.
;; In addition d, q, and r are consecutive positive integer terms in a geometric sequence,
;; but not necessarily in that order.

;; For example, 58 divided by 6 has quotient 9 and remainder 4.
;; It can also be seen that 4, 6, 9 are consecutive terms in a geometric sequence (common ratio 3/2).
;; We will call such numbers, n, progressive.

;; Some progressive numbers, such as 9 and 10404 = 102^2, happen to also be perfect squares.
;; The sum of all progressive perfect squares below one hundred thousand is 124657.

;; Find the sum of all progressive perfect squares below one trillion (10^12).


(defn explore [c]
  (->> (iterate inc 1)
       (mapcat (fn [m]
                 (let [n (* m m)]
                   (->> (range 1 m)
                        (map (fn [d]
                               [m
                                n
                                (mod n d)
                                d
                                (quot n d)]))))))
       (filter (fn [[m n r d q]]
                 (and (> r 0)
                      (== (/ d r) (/ q d)))))
       (take c)
       (map println)
       ))

(def first-pn
  [
   [3 9 1 2 4]
   [102 10404 36 72 144]
   [130 16900 25 75 225]
   [312 97344 8 92 1058]
   [759 576081 81 360 1600]
   [2496 6230016 512 1472 4232]
   [2706 7322436 1936 2420 3025]
   [3465 12006225 1225 2450 4900]
   [6072 36869184 5184 5760 6400]
   [6111 37344321 3969 5292 7056]
   [8424 70963776 5832 7452 9522]
   [14004 196112016 432 4392 44652]
   [16005 256160025 1089 6534 39204]
   [36897 1361388609 21609 30870 44100]
   [37156 1380568336 12544 25872 53361]
   [92385 8534988225 50625 75600 112896]
   [98640 9729849600 50625 78975 123201]
   [112032 12551169024 27648 70272 178608]
   [117708 13855173264 41616 83232 166464]
   [128040 16394241600 69696 104544 156816]
   ])

(defn search-from [m d]
  (->>
   (iterate inc m)
   (map
    (fn [m]
      (let [n (* m m)]
        (->> (range d m)
             (map (fn [d]
                    [m
                     n
                     (mod n d)
                     d
                     (quot n d)]))
             (filter (fn [[m n r d q]]
                       (and (> r 0)
                            (== (/ d r) (/ q d)))))
             first))))
   (remove nil?)
   first))

(defn explore-from [m d]
  (lazy-seq
   (let [[m n r d q] (search-from m d)]
     (cons [m n r d q] (explore-from (inc m) d)))))

(defn- gcd2 [a b]
  (cond (zero? b) a
        :else     (recur b (mod a b))))

(defn gcd [a & bs] (reduce gcd2 a bs))

(defn process-1st []
  (->> first-pn
       (map (fn [[m n r d q]]
              (let [k (gcd n r d q)]
                [k (/ n k) (/ r k) (/ d k) (/ q k)])))
       (map println)
       ))

#_(defn explore [c]
  (->>
   (explore-from 1 1)
   (take c)
   (map println)
   )
    )

(defn solution []
  (->> (for [r1 (range 1 1000001)
             r2 (range 1 1001)
             :let [r (* r1 r2 r2)]
             :while (< r 1000001)
             k (range 1 1001 (/ 1 r2))
             :let [n (+ r (* k k k r r))]
             :while (< n 1e12)
             :let [m (int (Math/sqrt n))]
             :when (== n (* m m))]
         n)
       (reduce conj #{})
       (apply +)
       ))

;; 878454337159
