(require '[clojure.string :as str])

(defn score-round [round]
  (let [a (case (nth round 2)
                   \X 0
                   \Y 3
                   \Z 6)
        b (case round
            "A X" 3
            "A Y" 1
            "A Z" 2

            "B X" 1
            "B Y" 2
            "B Z" 3

            "C X" 2
            "C Y" 3
            "C Z" 1
            0)]
    (+ a b)))


(->> (str/split (slurp "day2-input.txt") #"\n")
     (map score-round)
     (reduce +)
     (println))
