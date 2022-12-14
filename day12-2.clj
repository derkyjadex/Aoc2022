(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn moves [heights [x y]]
  (let [h (dec (nth (nth heights y) x))
        dirs [[x (dec y)]
              [x (inc y)]
              [(dec x) y]
              [(inc x) y]]]
    (set (filter (fn [[x' y']]
                   (<= h (nth (nth heights y' []) x' 0)))
                 dirs))))

(defn init [input]
  (let [input (str/replace input "S" "a")
        lines (str/split-lines input)
        heights (->> (str/replace input "E" "z")
                     (str/split-lines)
                     (mapv #(mapv long %)))
        starts (for [y (range (count heights))
                     x (range (count (first heights)))
                     :when (= (long \a) (nth (nth heights y) x))]
                 [x y])
        end (let [x (map #(str/index-of % \E) lines)]
              [(some identity x)
               (count (take-while not x))])]
    {:edges (->> (for [y (range (count heights))
                       x (range (count (first heights)))]
                   [x y])
                 (map #(vector % (moves heights %)))
                 (into {}))
     :starts starts
     :visited #{}
     :distances {end 0}
     :pos end}))

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
        options (filter #(not (contains? visited' (first %))) distances')
        pos' (and (seq options)
                  (first (apply min-key second options)))]
    (assoc state
           :visited visited'
           :distances distances'
           :pos pos')))

(defn result [{starts :starts distances :distances}]
  (->> starts
       (map #(get distances %))
       (filter some?)
       (sort)
       (first)))

(->> (slurp "day12-input.txt")
     (init)
     (iterate solve)
     (drop-while #(some? (:pos %)))
     (first)
     (result)
     (println))
