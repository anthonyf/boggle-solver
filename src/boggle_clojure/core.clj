;;;; BOGGLE SOLVER ;;;;

(ns boggle-clojure.core
  (:require [clojure.string :refer :all])
  (:gen-class))

;;; BOGGLE BOARD ;;;

(def dice
  "The official boggle dice."
  '[[A  A  E  E  G  N]
    [E  L  R  T  T  Y]
    [A  O  O  T  T  W]
    [A  B  B  J  O  O]
    [E  H  R  T  V  W]
    [C  I  M  O  T  U]
    [D  I  S  T  T  Y]
    [E  I  O  S  S  T]
    [D  E  L  R  V  Y]
    [A  C  H  O  P  S]
    [H  I  M  N  Qu U] ;; note Qu (two letters)
    [E  E  I  N  S  U]
    [E  E  G  H  N  W]
    [A  F  F  K  P  S]
    [H  L  N  N  R  Z]
    [D  E  I  L  R  X]])

(defn make-boggle-board
  []
  (mapv #(into [] %)
        (partition 4 (mapv rand-nth (shuffle dice)))))

(defn boggle-board-indices
  "Return a vector of all [x y] indices on a boggle board."
  []
  (reduce (fn [result x]
            (reduce (fn [result y]
                      (conj result [x y]))
                    result
                    (range 4)))
          []
          (range 4)))

(defn boggle-board-letter-at-pos
  "Retrieve letter at given boggle board position."
  [board [x y]]
  ((board y) x))

(defn print-boggle-board
  [board]
  (dotimes [x 4]
    (dotimes [y 4]
      (print (format "%3s" (boggle-board-letter-at-pos board [x y]))))
    (println)))

;;; DICTIONARY ;;;

(defn word-to-symbol-array
  "Coverts a string of characters to a vector of upcased letters as symbols,
  e.g. 'Water' => [W A T E R]. This will make processing the data easier."
  [word]
  (mapv (fn [letter] (symbol (str letter)))
        (upper-case word)))

(defn replace-qu
  "Replaces words containing adjacent Q and U with a single Qu. This will fix up
  the words to handle the Qu on the boggle dice."
  [word]
  (loop [letter (first word)
         left (rest word)
         new-word []]
    (if (nil? letter)
      new-word
      (if (and (= letter 'Q)
               (= (first left) 'U))
        (recur (first (rest left))
               (rest (rest left))
               (conj new-word 'Qu))
        (recur (first left)
               (rest left)
               (conj new-word letter))))))

(defn load-words
  "Loads words.txt info a giant vector of words where each word a vector of
  letter symbols."
  []
  (with-open [in (clojure.java.io/reader
                  "resources/words.txt"
                  ;;"resources/ospd.txt"
                  )]
    (mapv (comp replace-qu word-to-symbol-array)
          (line-seq in))))

(defn add-word-to-dict
  "Adds a word (vector of symbols) to the dictionary.  The dictionary is
  reprecentded by a prefix trie."
  [dict word]
  (assoc-in dict word (merge (get-in dict word)
                             {:word? true})))

(defn make-dict
  "Makes a dictionary from a list of words."
  ([]
   (make-dict (load-words)))
  ([words]
   (reduce add-word-to-dict {} words)))

;;; BOGGLE SOLVER ;;;

(defn- boggle-board-possible-next-positions
  "Returns next possible boggle board positions that are not in `visited`."
  [[x y] visited]
  (reduce (fn [valid [offset-x offset-y]]
            (let [nx (+ x offset-x)
                  ny (+ y offset-y)]
              (if (and (>= nx 0)
                       (< nx 4)
                       (>= ny 0)
                       (< ny 4)
                       (not (contains? visited [nx ny])))
                (conj valid [nx ny])
                valid)))
          []
          ;; index offsets
          [[0 1] [ 1 0] [0 -1] [-1  0]
           [1 1] [-1 1] [1 -1] [-1 -1]]))

(defn- solve-for-starting-position
  "Solve boggle board from given starting position using depth first search."
  [board dict pos found-words letters-so-far visited]
  (let [letter (boggle-board-letter-at-pos board pos)
        possible-word (conj letters-so-far letter)]
    (if (not (nil? (get dict letter)))
      (let [visited (conj visited pos)
            dict (get dict letter)
            found-words (if (:word? dict false)
                          (conj found-words possible-word)
                          found-words)]
        (reduce (fn [found-words new-pos]
                  (solve-for-starting-position board
                                               dict
                                               new-pos
                                               found-words
                                               possible-word
                                               visited))
                found-words
                (boggle-board-possible-next-positions pos visited)))
      found-words)))

(defn boggle-words
  "Find all boggle words for given boggle board."
  ([]
   (boggle-words (make-boggle-board)))
  ([board]
   (let [dict (make-dict)]
     (reduce (fn [found-words start-pos]
               (solve-for-starting-position
                board dict start-pos found-words [] #{}))
             #{}
             (boggle-board-indices)))))

(defn -main
  [& args]
  (let [board (make-boggle-board)
        words (boggle-words board)]
    (println "board:")
    (print-boggle-board board)    
    (println (format "\nwords found(%d):" (count words)))
    (doseq [word words]
      (println (lower-case (apply str word))))))

;;; SAMPLE OUTPUT ;;;

;; boggle-clojure.core> (-main)
;; board:
;;   P  Y  A  W
;;   L  G  L  S
;;   M  R  O  N
;;   E Qu  S  O

;; words found(128):
;; ag
;; al
;; as
;; aw
;; ay
;; em
;; er
;; go
;; la
;; lo
;; me
;; no
;; on
;; or
;; os
;; re
;; so
;; ya
;; ago
;; als
;; awl
;; erg
;; ers
;; gal
;; gas
;; gay
;; goo
;; gor
;; gos
;; gyp
;; lag
;; las
;; law
;; lay
;; log
;; loo
;; nog
;; noo
;; nor
;; nos
;; ono
;; ons
;; ore
;; ors
;; ply
;; pya
;; rem
;; sag
;; sal
;; saw
;; say
;; sly
;; sol
;; son
;; sos
;; wag
;; was
;; way
;; yag
;; yaw
;; agly
;; agon
;; also
;; awls
;; ergo
;; eros
;; gals
;; goon
;; goos
;; gore
;; gorm
;; laws
;; logy
;; loon
;; loos
;; lore
;; merl
;; norm
;; ogre
;; only
;; onos
;; orgy
;; pyas
;; roque
;; sago
;; sagy
;; slag
;; slaw
;; slay
;; slog
;; snog
;; sola
;; sols
;; sons
;; soon
;; sore
;; swag
;; sway
;; waly
;; yawl
;; yaws
;; agons
;; algor
;; goons
;; loons
;; merls
;; onlay
;; pylon
;; sagos
;; salon
;; snool
;; snore
;; sonly
;; wagon
;; yawls
;; algors
;; galore
;; lagoon
;; merlon
;; pylons
;; salons
;; saloon
;; sawlog
;; snools
;; wagons
;; lagoons
;; merlons
;; saloons
;; nil
;; boggle-clojure.core> 
