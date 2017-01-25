(ns project-euler.problem_0121)

;; A bag contains one red disc and one blue disc.
;; In a game of chance a player takes a disc at random and its colour is noted.
;; After each turn the disc is returned to the bag, an extra red disc is added,
;; and another disc is taken at random.

;; The player pays £1 to play and wins if they have taken more blue discs than red discs at the end of the game.

;; If the game is played for four turns, the probability of a player winning is exactly 11/120,
;; and so the maximum prize fund the banker should allocate for winning in this game
;; would be £10 before they would expect to incur a loss.
;; Note that any payout will be a whole number of pounds
;; and also includes the original £1 paid to play the game,
;; so in the example given the player actually wins £9.

;; Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played.

(defn sim-turn [bag]
  [(rand-nth bag) (conj bag :r)])

(defn sim-n-turn [n]
  (loop [bag [:r :b]
         n n
         score 0]
    (if (zero? n)
      score
      (let [[d bag'] (sim-turn bag)]
        (recur bag'
               (dec n)
               (if (= d :b) (inc score) score)))
      )))

(defn sim-m-game [n m]
  (let [win-level (/ n 2)]
    (->> (repeatedly m (partial sim-n-turn n))
         (filter #(> % win-level))
         (count)
         )))

;;---------------------------------------------------------------------------

(defn win-probs [l n d]
  (cond (zero? l) 0
        (zero? n) 1
        :else     (+ (/ (win-probs l       (dec n) (inc d)) d)
                     (* (win-probs (dec l) (dec n) (inc d)) (/ (dec d) d)))))

(defn solution []
  (int (/ (win-probs 8 15 2))))

;;-> 2269


