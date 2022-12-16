(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-path [line]
  (->> (str/split line #" -> ")
       (map #(map read-string (str/split % #",")))
       (partition 2 1)
       (map (fn [[[sx sy] [ex ey]]]
                 (if (= sx ex)
                   (map #(vector sx %) (range (min sy ey) (inc (max sy ey))))
                   (map #(vector % sy) (range (min sx ex) (inc (max sx ex))))
                   )))
       (apply concat)))

(defn read-scan [input]
  (->> (str/split-lines input)
       (map read-path)
       (apply concat)
       (set)))

(defn init [input]
  (let [rock (read-scan input)]
    {:rock rock
     :maxy (apply max (map second rock))
     :sand 0
     :pos [500 0]}))

(defn step [{rock :rock [x y] :pos :as state}]
  (cond
    (not (contains? rock [x (inc y)]))
    (assoc state :pos [x (inc y)])
    
    (not (contains? rock [(dec x) (inc y)]))
    (assoc state :pos [(dec x) (inc y)])

    (not (contains? rock [(inc x) (inc y)]))
    (assoc state :pos [(inc x) (inc y)])
    
    :else
    (assoc state
           :rock (conj rock [x y])
           :sand (inc (:sand state))
           :pos [500 0])))

(->> (slurp "day14-input.txt")
     (init)
     (iterate step)
     (drop-while #(> (:maxy %) (second (:pos %))))
     (first)
     (:sand)
     (println))
