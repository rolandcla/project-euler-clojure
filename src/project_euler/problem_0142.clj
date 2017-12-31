(ns project-euler.problem_0142)

;; Find the smallest x + y + z with integers x > y > z > 0 such that
;; x + y, x − y, x + z, x − z, y + z, y − z are all perfect squares.

(defn perfect-sq? [n] (let [m (int (Math/sqrt n))] (== n (* m m))))

(defn solution []
  (->> (for [s (iterate inc 1)
             z (range 1 (quot s 3))
             y (range (inc z) (quot (- s z -1) 2))
             :let [x (- s z y)]
             ]
         [x y z])
       (filter (fn [[x y z]] (and (perfect-sq? (+ x y))
                                  (perfect-sq? (- x y))
                                  (perfect-sq? (+ x z))
                                  (perfect-sq? (- x z))
                                  (perfect-sq? (+ y z))
                                  (perfect-sq? (- y z)))))
       (take 1)
       ))

;; from wikipedia https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

(defn ppt-children [[a b c]]
  [(let [a-2b+2c (- (+ a c c) b b)]
     [a-2b+2c
      (+ a-2b+2c a b)
      (+ a-2b+2c a c)])
   (let [a+b+2c (+ a b c c)]
     [(+ a+b+2c b)
      (+ a+b+2c a)
      (+ a+b+2c a b c)])
   (let [b+2c-2a (- (+ b c c) a a)]
     [(+ b+2c-2a a b)
      b+2c-2a
      (+ b+2c-2a b c)])])

(defn generate-ppt [s-max src]
  (let [s (apply + src)]
    (if (< s s-max)
      (cons [src s] (->> (ppt-children src)
                        (mapcat #(generate-ppt s-max %)))))))

(defn generate-pt [s-max]
  (->> (generate-ppt s-max [3 4 5])
       (mapcat (fn [[ppt s]] (->> (range 1 (inc (quot s-max s)))
                                  (map (fn [k] (map #(* k %) ppt))))))))

(defn explore [s-max]
  (->> (generate-pt s-max)
       (map sort)
       (mapcat (fn [[f c a]]
                 (let [a2 (* a a)
                       c2 (* c c)]
                   (->> (range 1 c)
                        (filter (fn [e] (let [e2 (* e e)]
                                          (and (perfect-sq? (- a2 e2))
                                               (perfect-sq? (- c2 e2))))))
                        (map (fn [e] (let [e2 (* e e)
                                           d2 (- a2 e2)
                                           b2 (- c2 e2)
                                           f2 (* f f)]
                                       [a2 b2 c2 d2 e2 f2])))))))
       (map (fn [[a2 b2 c2 d2 e2 f2]] (/ (+ a2 c2 e2) 2)))
       (sort)
       (filter integer?)
       first
       ))

(defn solution [] (explore 10000))

;; 1006193
