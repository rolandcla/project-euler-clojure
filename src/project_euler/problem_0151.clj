(ns project-euler.problem_0151)

;; A printing shop runs 16 batches (jobs) every week and each batch requires
;; a sheet of special colour-proofing paper of size A5.

;; Every Monday morning, the foreman opens a new envelope,
;; containing a large sheet of the special paper with size A1.

;; He proceeds to cut it in half, thus getting two sheets of size A2.
;; Then he cuts one of them in half to get two sheets of size A3 and so on until he obtains
;; the A5-size sheet needed for the first batch of the week.

;; All the unused sheets are placed back in the envelope.

;; At the beginning of each subsequent batch, he takes from the envelope one sheet of paper at random.
;; If it is of size A5, he uses it.
;; If it is larger, he repeats the 'cut-in-half' procedure until he has what he needs
;; and any remaining sheets are always placed back in the envelope.

;; Excluding the first and last batch of the week,
;; find the expected number of times (during each week)
;; that the foreman finds a single sheet of paper in the envelope.

;; Give your answer rounded to six decimal places using the format x.xxxxxx .

(def cut {:a1 [:a2 :a3 :a4 :a5]
          :a2 [:a3 :a4 :a5]
          :a3 [:a4 :a5]
          :a4 [:a5]
          :a5 []})

(defn one-batch [envelope-probs]
  (->> envelope-probs
       (mapcat (fn [[sheets chance]]
              (let [n (count sheets)]
                (->> (range n)
                     (map (fn [ix] [(->> (update sheets ix cut)
                                          flatten
                                          sort
                                          reverse
                                          vec)
                                    (/ chance n)]))))))
       (reduce (fn [m [shts n]] (update m shts (fn [x] (if x (+ n x) n))))
               {})))

(defn batch-seq []
  (->> {[:a1] 1}
       (iterate one-batch)
       (take 16)))

(defn single-sheet-chance [envelope-probs]
  (->> envelope-probs
       (map (fn [[sheets chance]] (if (== 1 (count sheets)) chance 0)))
       (apply +)))

(defn solution []
  (->> (batch-seq)
       (drop 1)
       (take 14)
       (map single-sheet-chance)
       (apply +)
       double))

;; 0.4643987816010871
