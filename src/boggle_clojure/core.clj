;; http://codegolf.stackexchange.com/questions/5654/best-scoring-boggle-board

(ns boggle-clojure.core
  (:gen-class))

(def dice '[[A  A  E  E  G  N]
            [E  L  R  T  T  Y]
            [A  O  O  T  T  W]
            [A  B  B  J  O  O]
            [E  H  R  T  V  W]
            [C  I  M  O  T  U]
            [D  I  S  T  T  Y]
            [E  I  O  S  S  T]
            [D  E  L  R  V  Y]
            [A  C  H  O  P  S]
            [H  I  M  N  Qu U]
            [E  E  I  N  S  U]
            [E  E  G  H  N  W]
            [A  F  F  K  P  S]
            [H  L  N  N  R  Z]
            [D  E  I  L  R  X]])

(def words (with-open [in (clojure.java.io/reader "resources/words.txt")]
             (into [] (line-seq in))))

(def dict (reduce (fn [map word]
                    )
                  {}
                  words))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
