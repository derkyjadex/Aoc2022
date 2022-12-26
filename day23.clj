(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn propose [[x y] elves round]
  (let [nw [(dec x) (dec y)]
        n [x (dec y)]
        ne [(inc x) (dec y)]
        e [(inc x) y]
        se [(inc x) (inc y)]
        s [x (inc y)]
        sw [(dec x) (inc y)]
        w [(dec x) y]]
    (if (not-any? elves [n ne e se s sw w nw])
      nil
      (->> [[n nw ne]
            [s sw se]
            [w nw sw]
            [e ne se]]
           (cycle)
           (drop (mod round 4))
           (take 4)
           (filter #(not-any? elves %))
           (first)
           (first)))))

(defn count-empty [elves]
  (let [min-x (apply min (map first elves))
        max-x (apply max (map first elves))
        min-y (apply min (map second elves))
        max-y (apply max (map second elves))]
    (- (* (- max-x min-x -1) (- max-y min-y -1))
       (count elves))))

(defn step [elves round]
  (let [proposed (->> elves
                      (map #(vector (propose % elves round) %))
                      (filter second))
        positions (into {} proposed)]
    (->> proposed
         (map first)
         (frequencies)
         (filter #(= (second %) 1))
         (map first)
         (reduce #(conj (disj %1 (get positions %2)) %2) elves))))

(defn init [input]
  (->> input
       (str/split-lines)
       (keep-indexed
         (fn [y line]
           (keep-indexed
             (fn [x c]
               (if (= c \#) [x y]))
             (char-array line))))
       (apply concat)
       (set)
       ))

(->> (reduce step (init (slurp "day23-input.txt")) (range 10))
     (count-empty)
     (println))
