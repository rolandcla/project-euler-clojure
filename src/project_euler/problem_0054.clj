(ns project-euler.problem_0054)

;; In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

;; If two players have the same ranked hands then the rank made up of the highest value wins;
;; for example, a pair of eights beats a pair of fives (see example 1 below).
;; But if two ranks tie, for example, both players have a pair of queens,
;; then highest cards in each hand are compared (see example 4 below);
;; if the highest cards tie then the next highest cards are compared, and so on.

;; Consider the following five hands dealt to two players:

;; Hand  	 	   Player 1	 	          Player 2	 	      Winner
;;-------------------------------------------------------------
;; 1	 	    5H 5C 6S 7S KD       2C 3S 8S 8D TD      Player 2
;;           Pair of Fives       Pair of Eights
;;
;; 2	 	    5D 8C 9S JS AC       2C 5C 7D 8S QH    	 Player 1
;;         Highest card Ace     Highest card Queen
;;
;; 3	    	2D 9C AS AH AC       3D 6D 7D TD QD      Player 2
;;            Three Aces       Flush with Diamonds
;;
;; 4	    	4D 6S 9H QH QC       3D 6D 7H QD QS      Player 1
;;          Pair of Queens       Pair of Queens
;;         Highest card Nine    Highest card Seven
;;
;; 5	    	2H 2D 4C 4D 4S       3C 3D 3S 9S 9D      Player 1
;;           Full House            Full House
;;         With Three Fours     with Three Threes

;; The file, poker.txt, contains one-thousand random hands dealt to two players.
;; Each line of the file contains ten cards (separated by a single space):
;; the first five are Player 1's cards and the last five are Player 2's cards.
;; You can assume that all hands are valid (no invalid characters or repeated cards),
;; each player's hand is in no specific order, and in each hand there is a clear winner.

;; How many hands does Player 1 win?

(def card-valid-suits (set "HCSD"))
(def card-valid-values (set  "23456789TJQKA"))
(def card-order (zipmap "23456789TJQKA" (iterate inc 2)))

(defn card-valid? [c] (and (string? c)
                           (== 2 (count c))
                           (card-valid-values (get c 0))
                           (card-valid-suits  (get c 1))))

(defn card-value [c]
  {:pre [(card-valid? c)]}
  (get c 0))

(defn card-suit [c]
  {:pre [(card-valid? c)]}
  (get c 1))

(defn card-cmp [c1 c2]
  {:pre [(card-valid? c1) (card-valid? c2)]}
  (compare (card-order (card-value c1))
           (card-order (card-value c2))))

;; Hands --------------------------------------------------------

(defn h-high-card [hand]
  (vec (sort > (map  (comp card-order card-value) hand))))

(defn h-one-pair [hand]
  (let [groups (->> (group-by card-value hand)
                   (filter (fn [[v grp]] (== 2 (count grp))))
                   )]
    (when (== 1 (count groups))
      (let [pair (second (first groups))]
        (mapcat h-high-card [pair (remove (set pair) hand)])))))

(defn h-two-pairs [hand]
  (let [groups (->> (group-by card-value hand)
                   (filter (fn [[v grp]] (== 2 (count grp))))
                   )]
    (when (== 2 (count groups))
      (let [pairs (mapcat (fn [pair] (second pair)) groups)]
        (mapcat h-high-card [pairs (remove (set pairs) hand)])))))

(defn h-three-of-a-kind [hand]
  (let [groups (->> (group-by card-value hand)
                   (filter (fn [[v grp]] (== 3 (count grp))))
                   )]
    (when (== 1 (count groups))
      (let [three (second (first groups))]
        (mapcat h-high-card [three (remove (set three) hand)])))))

(defn h-straight [hand]
  (let [[v & _ :as vals] (h-high-card hand)]
    (when (= vals (range v (- v 5) -1))
      vals)))

(defn h-flush [hand]
  (when (apply = (map card-suit hand))
    (h-high-card hand))
  )

(defn h-full-house [hand]
  (when-let [three (h-three-of-a-kind hand)]
    (when-let [pair (h-one-pair hand)]
      (concat (take 3 three) (take 2 pair)))))

(defn h-four-of-a-kind [hand]
  (let [groups (->> (group-by card-value hand)
                    (filter (fn [[v grp]] (== 4 (count grp))))
                    )]
    (when (== 1 (count groups))
      (let [four (second (first groups))]
        (mapcat h-high-card [four (remove (set four) hand)])))))

(defn h-straight-flush [hand]
  (and (h-flush hand) (h-straight hand)))

(defn h-royal-flush [hand]
  (when-let [vals (h-straight-flush hand)]
    (when (== (first vals) (card-order \A))
      vals)))

(def ranks [h-royal-flush
            h-straight-flush
            h-four-of-a-kind
            h-full-house
            h-flush
            h-straight
            h-three-of-a-kind
            h-two-pairs
            h-one-pair
            h-high-card
            ])

(defn h-rank [hand]
  (some identity (map (fn [f r] (when-let [vals (f hand)] [r (vec vals)])) ranks (iterate dec (count ranks)))))

(defn solution []
  (->> (slurp "resources/p054_poker.txt")
       (clojure.string/split-lines)
       (map (fn [line] (partition 5 (clojure.string/split line #" "))))
       (map (fn [[h1 h2]] [(h-rank h1) (h-rank h2)]))
       (map (fn [[r1 r2]] (compare r1 r2)))
       (filter #{1})
       (count)
       ))
