(require '[clojure.string :as str])

(->> (str/split (slurp "day1-input.txt") #"\n\n")
     (map (fn [elf-items]
            (->> (str/split elf-items #"\n")
                 (map #(Integer/parseInt %))
                 (reduce +))))
     (sort)
     (take-last 3)
     (reduce +)
     (println))
