(ns project-euler.problem-0083)

;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
;; by moving left, right, up, and down, is indicated in bold red and is equal to 2297.

;; Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
;; a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left,
;; right, up, and down.

(def s-matrix [[131 673 234 103  18]
               [201  96 342 965 150]
               [630 803 746 422 111]
               [537 699 497 121 956]
               [805 732 524  37 331]])

(defn init-distance-map [matrix start]
  (->> matrix
       (map-indexed (fn [row-ix row]
                      (map-indexed (fn [col-ix v] [row-ix col-ix v]) row)))
       (apply concat)
       (reduce (fn [[d-set d-map] [row-ix col-ix v]]
                 (let [d (if (= [row-ix col-ix] start) (get-in matrix start) Double/POSITIVE_INFINITY)]
                   [(conj d-set [d row-ix col-ix])
                    (assoc d-map [row-ix col-ix] d)]))
               [(sorted-set) {}])
       ))

(defn peek-distance-map [[d-set _]] (first d-set))

(defn pop-distance-map [[d-set d-map]]
  (let [[d r c] (first d-set)]
    [(disj d-set [d r c])
     (dissoc d-map [r c])]))

(defn push-distance-map [[d-set d-map] [d r c]]
  [(conj d-set [d r c])
   (assoc d-map [r c] d)])

(defn get-distance-map [[_ d-map] [r c]] (get d-map [r c]))

(defn update-distance-map [[d-set d-map] [r c] d]
  (let [d' (get d-map [r c])]
    (if (< d d')
      [(-> d-set
           (disj [d' r c])
           (conj [d r c]))
       (assoc d-map [r c] d)]
      [d-set d-map]
      )))

(defn contains-distance-map? [[_ d-map] rc] (contains? d-map rc))

(defn neighbors [size r c]
  (->> [[r (+ c 1)] [r (- c 1)] [(+ r 1) c] [(- r 1) c]]
       (filter (fn [[r' c']] (and (< -1 r' size) (< -1 c' size))))
       ))

(defn dijkstra [matrix start end]
  (let [size (count matrix)]
    (loop [u (init-distance-map matrix start)]
      (let [[d r c] (peek-distance-map u)]
        (if (= [r c] end)
          d
          (recur (->> (neighbors size r c)
                      (filter (fn [rc] (contains-distance-map? u rc)))
                      (reduce (fn [u' [r' c']]
                                (update-distance-map u' [r' c'] (+ d (get-in matrix [r' c']))))
                              (pop-distance-map u))
                      ))
          )))))

(defn solution []
  (->> (slurp "resources/p083_matrix.txt")
       (clojure.string/split-lines)
       (map (fn [line] (vec (map (comp int bigdec) (clojure.string/split line #",")))))
       (vec)
       ((fn [matrix] (dijkstra matrix [0 0] [(dec (count matrix)) (dec (count matrix))])))
       ))

;;-> 425185
