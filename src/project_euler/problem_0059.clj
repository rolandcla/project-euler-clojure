(ns project-euler.problem_0059)

;; Each character on a computer is assigned a unique code and the preferred standard is ASCII
;; (American Standard Code for Information Interchange).
;; For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

;; A modern encryption method is to take a text file, convert the bytes to ASCII,
;; then XOR each byte with a given value, taken from a secret key.
;; The advantage with the XOR function is that using the same encryption key on the cipher text,
;; restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

;; For unbreakable encryption, the key is the same length as the plain text message,
;; and the key is made up of random bytes.
;; The user would keep the encrypted message and the encryption key in different locations,
;; and without both "halves", it is impossible to decrypt the message.

;; Unfortunately, this method is impractical for most users,
;; so the modified method is to use a password as a key.
;; If the password is shorter than the message, which is likely,
;; the key is repeated cyclically throughout the message.
;; The balance for this method is using a sufficiently long password key for security,
;; but short enough to be memorable.

;; Your task has been made easy, as the encryption key consists of three lower case characters.
;; Using cipher.txt (right click and 'Save Link/Target As...'),
;; a file containing the encrypted ASCII codes,
;; and the knowledge that the plain text must contain common English words,
;; decrypt the message and find the sum of the ASCII values in the original text.

(defn histogram []
  (->> (clojure.string/split (slurp "resources/p059_cipher.txt") #",")
       (map #(Integer. (clojure.string/trim  %)))
       (partition-all 3)
       (map vec)
       (#(for [i (range 3)] (for [v3 % :let [v (get v3 i)] :when v] v)))
       (map (fn [xs] (->> (group-by identity xs)
                          (map (fn [[k v]] [k (count v)]))
                          (sort-by second >)
                          (take 4)
                          (vec))))
       (vec)
       ))

(defn decode [k1 k2 k3]
  (->> (clojure.string/split (slurp "resources/p059_cipher.txt") #",")
       (map #(Integer. (clojure.string/trim  %)))
       (map (fn [k v] (bit-xor k v)) (cycle [k1 k2 k3]))
  ))

(defn solution []
  (let [hist (histogram)
        k1   (bit-xor (int \space) (get-in hist [0 0 0]))
        k2   (bit-xor (int \space) (get-in hist [1 0 0]))
        k3   (bit-xor (int \space) (get-in hist [2 0 0]))
        mess (decode k1 k2 k3)]
    (println (apply str (map char mess)))
    (apply + mess)
    ))

;;-> 107359
