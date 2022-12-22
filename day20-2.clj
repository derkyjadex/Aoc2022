(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn move [lines i]
  (let [[before [[i v] & after]] (split-with #(not= i (first %)) lines)
        x (mod v (dec (count lines)))
        num-after (count after)]
    (into [] (if (<= x num-after)
               (let [[middle end] (split-at x after)]
                 (concat before middle [[i v]] end))
               (let [[start middle] (split-at (- x num-after) before)]
                 (concat start [[i v]] middle after))))))

(let [lines (->> (slurp "day20-input.txt")
                 (str/split-lines)
                 (map read-string)
                 (map-indexed #(vector %1 (* 811589153 %2))))
      n (count lines)
      mixed (->> lines
                 (iterate #(reduce move % (range 0 n)))
                 (drop 10)
                 (first)
                 (map second)
                 (into []))
      offset (.indexOf mixed 0)
      result (+ (nth mixed (mod (+ 1000 offset) n))
                (nth mixed (mod (+ 2000 offset) n))
                (nth mixed (mod (+ 3000 offset) n)))]
  (println result))
