(ns project-euler.problem_0107
  (:require [clojure.set :as set])
  (:require [jordanlewis.data.union-find :as uf]))

;; The following undirected network consists of seven vertices and twelve edges with a total weight of 243.

;; The same network can be represented by the matrix below.

;;    0  1  2  3  4  5  6
;;    A  B  C  D  E  F  G
;; A  -	 16	12 21 -  -  -
;; B  16 -  -  17 20 -  -
;; C  12 -  -  28 -  31 -
;; D  21 17 28 -  18 19 23
;; E  -  20 -  18 -  -  11
;; F  -  -  31 19 -  -  27
;; G  -  -  -  23 11 27 -
;; However, it is possible to optimise the network by removing some edges
;; and still ensure that all points on the network remain connected.
;; The network which achieves the maximum saving is shown below.
;; It has a weight of 93, representing a saving of 243 − 93 = 150 from the original network.

;; Using network.txt (right click and 'Save Link/Target As...'),
;; a 6K text file containing a network with forty vertices, and given in matrix form,
;; find the maximum saving which can be achieved by removing redundant edges
;; whilst ensuring that the network remains connected.


(def g1 {0 {1 16, 2 12, 3 21}
         1 {0 16, 3 17, 4 20}
         2 {0 12, 3 28, 5 31}
         3 {0 21, 1 17, 2 28, 4 18, 5 19, 6 23}
         4 {1 20, 3 18, 6 11}
         5 {2 31, 3 19, 6 27}
         6 {3 23, 4 11, 5 27}})

(defn read-graph []
  (->> (slurp "resources/p107_network.txt")
       (clojure.string/split-lines)
       (map-indexed (fn [r line]
                      (->> (clojure.string/split line #",")
                           (map-indexed vector)
                           (remove (fn [[_ w]] (#{"-"} w)))
                           (map (fn [[c w]] [c (read-string w)]))
                           (reduce (fn [m [c w]] (assoc m c w)) {})
                           (vector r)
                           )))
       (reduce (fn [m [r ws]] (assoc m r ws)) {})
       ))

(defn graph-total-weight [graph]
  (->> (vals graph)
       (mapcat vals)
       (reduce +)
       ((fn [w] (/ w 2)))))

(defn graph-edges-ordered-by-w [graph]
  (sort (for [[v1 m] graph
              [v2 w] m]
          [w [v1 v2]])))

(defn kruskal [graph]
  (loop [f   (apply uf/union-find (keys graph))
         s   (graph-edges-ordered-by-w graph)
         mst []]
    (cond (empty? s)       mst
          (== 1 (count f)) mst
          :else (let [[w [v1 v2]] (first s)
                      [_ s1] (uf/get-canonical f v1)
                      [_ s2] (uf/get-canonical f v2)]
                  (if (= s1 s2)
                    (recur f (rest s) mst)
                    (recur (uf/union f v1 v2) (rest s) (conj mst [[v1 v2] w]))
                    ))

      )
    ))

(defn solution []
  (let [g (read-graph)]
    (->> (kruskal g)
         (map second)
         (reduce +)
         ((fn [w] (- (graph-total-weight g) w)))
         ))
  )

;;-> 259679
