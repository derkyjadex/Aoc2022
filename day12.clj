(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn moves [heights [x y]]
  (let [h (nth (nth heights y) x)
        dirs [[x (dec y)]
              [x (inc y)]
              [(dec x) y]
              [(inc x) y]]]
    (set (filter (fn [[x' y']]
                   (>= (inc h)
                       (nth (nth heights y' [1000]) x' 1000)))
                 dirs))))

(defn init [input]
  (let [lines (str/split-lines input)
        heights (->> (str/replace (str/replace input "S" "a") "E" "z")
                     (str/split-lines)
                     (mapv #(mapv long %)))
        start (let [x (map #(str/index-of % \S) lines)]
                [(some identity x)
                 (count (take-while not x))])]
    {:edges (->> (for [y (range (count heights))
                       x (range (count (first heights)))]
                   [x y])
                 (map #(vector % (moves heights %)))
                 (into {})
                 )
     :visited #{}
     :distances {start 0}
     :pos start
     :end (let [x (map #(str/index-of % \E) lines)]
            [(some identity x)
             (count (take-while not x))])}))

(defn solve [{distances :distances
              visited :visited
              pos :pos
              :as state}]
  (let [dist (inc (get distances pos))
        distances' (reduce (fn [ds p]
                             (update ds p #(min (or % Long/MAX_VALUE) dist)))
                           distances
                           (set/difference
                             (get (:edges state) pos)
                             visited))
        visited' (conj visited pos)
        pos' (->> distances'
                  (filter #(not (contains? visited' (first %))))
                  (sort-by second)
                  (first)
                  (first))]
    (-> state
        (assoc :visited visited')
        (assoc :distances distances')
        (assoc :pos pos'))))

(defn result [{distances :distances end :end}]
  (get distances end))

(->> (init (slurp "day12-input.txt"))
     (iterate solve)
     (drop-while #(not= (:pos %) (:end %)))
     (first)
     (result)
     (println))
