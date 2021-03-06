;;;; BOGGLE SOLVER ;;;;

(ns boggle-clojure.core
  (:require [clojure.string :refer :all])
  (:gen-class))

;;; BOGGLE BOARD ;;;

(def board-size 4)

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
  (mapv vec
        (partition board-size
                   (map rand-nth (shuffle dice)))))

(defn boggle-board-indices
  "Return a vector of all [x y] indices on a boggle board."
  []
  (for [x (range board-size) y (range board-size)] [x y]))

(defn boggle-board-letter-at-pos
  "Retrieve letter at given boggle board position."
  [board [x y]]
  ((board y) x))

(defn print-boggle-board!
  [board]
  (dotimes [x board-size]
    (dotimes [y board-size]
      (printf "%3s" (boggle-board-letter-at-pos board [x y])))
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
  (:letters
   (reduce (fn [{:keys [letters prev-letter]} letter]
             [(conj letters letter)
              letter]
             {:letters (if (and (= 'U letter)
                                (= prev-letter 'Q))
                         (conj (pop letters) 'Qu)
                         (conj letters letter))
              :prev-letter letter})
           {:letters [], :prev-letter nil}
           word)))

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
  represented by a prefix trie."
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
                       (< nx board-size)
                       (>= ny 0)
                       (< ny board-size)
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
    (if-not (nil? (letter dict))
      (let [visited (conj visited pos)
            dict (letter dict)
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

(defn find-boggle-words
  "Find all boggle words for given boggle board."
  ([]
   (find-boggle-words (make-boggle-board)))
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
        words (find-boggle-words board)]
    (println "board:")
    (print-boggle-board! board)
    (printf "\nwords found(%d):\n" (count words))
    (doseq [word (sort words)]
      (println (lower-case (apply str word))))))

;;; SAMPLE OUTPUT ;;;

;; boggle-clojure.core> (-main)
;; board:
;;   S  T  N  V
;;   H  O  I  S
;;   E  T  R  B
;;   S Qu  A  E

;; words found(403):
;; ab
;; ae
;; ar
;; at
;; ba
;; be
;; bi
;; eh
;; er
;; es
;; et
;; he
;; ho
;; in
;; is
;; it
;; no
;; oe
;; oh
;; oi
;; on
;; or
;; os
;; qua
;; re
;; sh
;; si
;; so
;; ta
;; ti
;; to
;; abs
;; arb
;; are
;; ars
;; art
;; ate
;; bar
;; bat
;; bin
;; bio
;; bis
;; bit
;; bra
;; bro
;; ear
;; eat
;; eon
;; era
;; ers
;; eta
;; eth
;; hes
;; het
;; hoe
;; hon
;; hos
;; hot
;; ins
;; ion
;; ire
;; its
;; nib
;; nit
;; noh
;; nor
;; nos
;; not
;; nth
;; oes
;; ohs
;; ons
;; ora
;; orb
;; ore
;; ors
;; ort
;; rat
;; reb
;; rib
;; rin
;; roe
;; rot
;; set
;; she
;; sib
;; sin
;; sir
;; sit
;; son
;; sot
;; sri
;; tab
;; tae
;; tar
;; the
;; tho
;; tin
;; tis
;; tit
;; toe
;; ton
;; tor
;; tot
;; vis
;; abri
;; aero
;; arbs
;; arts
;; ates
;; bare
;; bars
;; bate
;; bath
;; bats
;; bear
;; beat
;; bins
;; bint
;; bios
;; biro
;; bite
;; bits
;; brae
;; brat
;; brin
;; brio
;; bris
;; brit
;; bros
;; ears
;; eath
;; eats
;; eons
;; eros
;; eths
;; hest
;; hets
;; hoes
;; hons
;; hora
;; host
;; hots
;; into
;; ions
;; iota
;; iron
;; isba
;; nibs
;; nite
;; nits
;; noes
;; noir
;; nori
;; nosh
;; nota
;; note
;; orbs
;; orts
;; quare
;; quart
;; quate
;; quest
;; rate
;; rath
;; rato
;; rats
;; rebs
;; ribs
;; rins
;; riot
;; rite
;; roes
;; rota
;; rote
;; roti
;; rots
;; seta
;; shes
;; shoe
;; shot
;; sire
;; site
;; sith
;; sits
;; snib
;; snit
;; snot
;; sons
;; sora
;; sorb
;; sore
;; sori
;; sort
;; soth
;; sots
;; squab
;; squat
;; stab
;; star
;; stir
;; stot
;; tabs
;; tare
;; taro
;; tars
;; tins
;; tint
;; tire
;; tiro
;; tits
;; toes
;; toit
;; tons
;; tora
;; tore
;; tori
;; tors
;; tort
;; tosh
;; tost
;; tote
;; tots
;; trio
;; trot
;; vibe
;; vino
;; vita
;; abris
;; baron
;; barque
;; bates
;; bathe
;; baths
;; baton
;; bears
;; beats
;; berth
;; bints
;; biont
;; biota
;; biros
;; birth
;; bites
;; brats
;; brins
;; brios
;; brith
;; brits
;; broth
;; earth
;; ethos
;; irate
;; irons
;; nites
;; nitre
;; nitro
;; noirs
;; noris
;; north
;; notes
;; orate
;; orbit
;; quarte
;; quarto
;; quarts
;; quatre
;; rates
;; rathe
;; ratio
;; ratos
;; riots
;; rites
;; rotes
;; rotis
;; setae
;; seton
;; shoes
;; shore
;; short
;; shote
;; shots
;; sitar
;; sites
;; snits
;; snore
;; snort
;; snots
;; sorbs
;; sorta
;; sorts
;; squabs
;; square
;; stabs
;; stare
;; stars
;; stint
;; stirs
;; store
;; stots
;; taber
;; taros
;; tarot
;; tarsi
;; theta
;; tints
;; tiros
;; tithe
;; titre
;; toits
;; torque
;; torsi
;; torta
;; torte
;; torts
;; totes
;; tribe
;; trios
;; trois
;; troth
;; trots
;; vinos
;; vitae
;; aroint
;; arsino
;; barite
;; barons
;; barques
;; bathes
;; bathos
;; batons
;; berate
;; berths
;; bionts
;; births
;; breath
;; briths
;; broths
;; earths
;; equator
;; estrin
;; intort
;; nitros
;; norite
;; norths
;; noshes
;; orates
;; orbits
;; quartes
;; quartos
;; questor
;; ration
;; ratios
;; rebate
;; rebato
;; setons
;; shorts
;; shotes
;; snorts
;; stints
;; stotin
;; tabers
;; tarots
;; tithes
;; torques
;; tortes
;; toshes
;; triton
;; troths
;; tsoris
;; absinth
;; aroints
;; barites
;; berates
;; breathe
;; breaths
;; equation
;; equators
;; estrins
;; intorts
;; norites
;; oestrin
;; question
;; questors
;; rations
;; rebates
;; rebatos
;; sorbate
;; sorites
;; stotins
;; thorite
;; torquate
;; tritons
;; vibrate
;; vibrato
;; absinthe
;; absinths
;; breathes
;; oestrins
;; sorbates
;; thorites
;; vibrates
;; vibratos
;; nil
;; boggle-clojure.core>
