(require '[clojure.string :as str])

(defn score-round [round]
  (let [a (case (nth round 2)
                   \X 1
                   \Y 2
                   \Z 3)
        b (case round
            "A Y" 6
            "B Z" 6
            "C X" 6
            "A X" 3
            "B Y" 3
            "C Z" 3
            0)]
    (+ a b)))


(->> (str/split (slurp "day2-input.txt") #"\n")
     (map score-round)
     (reduce +)
     (println))
