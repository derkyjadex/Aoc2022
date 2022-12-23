(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn init [input]
  (let [[board steps] (str/split input #"\n\n")
        board (->> input
                   (str/split-lines)
                   (map-indexed
                     (fn [y line]
                       (keep-indexed
                         (fn [x c]
                           (case c
                             \. [[x y] :clear]
                             \# [[x y] :wall]
                             nil))
                         (char-array line))))
                   (apply concat)
                   (into {}))]
    {:board board
     :steps (->> steps
                 (re-seq #"\d+|[LR]")
                 (map read-string))
     :pos (->> (range)
               (map #(vector % 0))
               (filter #(get board %))
               (first))
     :dir [1 0]}))

(defn turn [current dir]
  (cond
    (= current [ 0 -1]) (if (= dir 'L) [-1  0] [+1  0])
    (= current [+1  0]) (if (= dir 'L) [ 0 -1] [ 0 +1])
    (= current [ 0 +1]) (if (= dir 'L) [+1  0] [-1  0])
    (= current [-1  0]) (if (= dir 'L) [ 0 +1] [ 0 -1])))

(defn take-step [[dx dy] [x y]]
  [(+ x dx) (+ y dy)])

(defn wrap [board [x y] [dx dy]]
  (->> [x y]
       (iterate #(take-step [(- dx) (- dy)] %))
       (drop-while #(some? (get board %)))
       (first)
       (take-step [dx dy])))

(defn run-step [{board :board
                 [step & steps'] :steps
                 pos :pos
                 dir :dir
                 :as state}]
  (if (symbol? step)
    (-> state
        (update :dir turn step)
        (assoc :steps steps'))

    (if (zero? step)
      (assoc state :steps steps')
      (let [pos' (take-step dir pos)
            pos' (if (nil? (get board pos'))
                   (wrap board pos dir)
                   pos')]
        (if (= :wall (get board pos'))
          (assoc state :steps steps')
          (-> state
              (assoc :pos pos')
              (assoc :steps (conj steps' (dec step)))))))))

(let [{[x y] :pos dir :dir :as state}
      (->> (slurp "day22-input.txt")
           (init)
           (iterate run-step)
           (drop-while #(some? (:steps %)))
           (first))
      dir (cond
            (= dir [ 0 -1]) 3
            (= dir [+1  0]) 0
            (= dir [ 0 +1]) 1
            (= dir [-1  0]) 2)]
  (println (+ (* 1000 (inc y)) (* 4 (inc x)) dir)))
