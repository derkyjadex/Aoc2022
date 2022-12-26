(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def face-size 50)

(def edges {:front [:top :right :bottom :left]
            :left [:top :front :bottom :back]
            :back [:top :left :bottom :right]
            :right [:top :back :bottom :front]
            :top [:back :right :front :left]
            :bottom [:front :right :back :left]})

(defn map-faces [face [x y]
                 parent parent-dir
                 all-faces net-map]
  (let [edges (->> (face edges)
                   (cycle)
                   (iterate rest)
                   (drop-while #(not= parent (nth % (+ parent-dir 2))))
                   (first)
                   (take 4)
                   (into []))
        net-map (-> net-map
                    (update :faces assoc [x y] face)
                    (update :edges assoc [x y] edges))]
    (->> [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]]
         (map vector edges)
         (map-indexed vector)
         (reduce (fn [net-map [dir [edge cell]]]
                   (if (and (get all-faces cell)
                            (not (get (:faces net-map) cell)))
                     (map-faces edge cell face dir all-faces net-map)
                     net-map))
                 net-map))))

(defn map-net [board]
  (let [faces (for [y (range 5)
                    x (range 5)
                    :when (get board [(* face-size x) (* face-size y)])]
                [x y])
        net-map (map-faces :front (first faces)
                           :top 2
                           (set faces) {:faces {} :edges {}})]
    (assoc net-map :cells (set/map-invert (:faces net-map)))))

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
     :net-map (map-net board)
     :steps (->> steps
                 (re-seq #"\d+|[LR]")
                 (map read-string))
     :pos (->> (range)
               (map #(vector % 0))
               (filter #(get board %))
               (first))
     :dir 1}))

(defn turn [current dir]
  (mod (+ current (case dir "L" -1  1)) 4))

(defn take-step [dir [x y]]
  (case (mod dir 4)
    0 [x (dec y)]
    1 [(inc x) y]
    2 [x (inc y)]
    3 [(dec x) y]))

(defn wrap [board net-map [x y] dir]
  (let [old-cell [(quot x face-size) (quot y face-size)]
        old-face (get (:faces net-map) old-cell)
        new-face (get (get (:edges net-map) old-cell) dir)
        coord (case dir
                0 (mod (- -1 x) face-size)
                1 (mod (- -1 y) face-size)
                2 (mod x face-size)
                3 (mod y face-size))
        [cell-x cell-y :as new-cell] (get (:cells net-map) new-face)
        new-dir (mod (+ 2 (.indexOf
                            (get (:edges net-map) new-cell)
                            old-face)) 4)
        new-pos (case new-dir
                  0 [(+ (- face-size coord 1) (* cell-x face-size))
                     (dec (* (inc cell-y) face-size))]
                  1 [(* cell-x face-size)
                     (+ (- face-size coord 1) (* cell-y face-size))]
                  2 [(+ coord (* cell-x face-size))
                     (* cell-y face-size)]
                  3 [(dec (* (inc cell-x) face-size))
                     (+ coord (* cell-y face-size))])]
    [new-pos new-dir]))

(defn run-step [{board :board
                 net-map :net-map
                 [step & steps'] :steps
                 pos :pos
                 dir :dir
                 :as state}]
  (if (symbol? step)
    (-> state
        (update :dir turn (str step))
        (assoc :steps steps'))

    (if (zero? step)
      (assoc state :steps steps')
      (let [pos' (take-step dir pos)
            [pos' dir'] (if (nil? (get board pos'))
                          (wrap board net-map pos dir)
                          [pos' dir])]
        (if (= :wall (get board pos'))
          (assoc state :steps steps')
          (-> state
              (assoc :pos pos')
              (assoc :dir dir')
              (assoc :steps (conj steps' (dec step)))))))))

(let [{[x y] :pos dir :dir :as state}
      (->> (slurp "day22-input.txt")
           (init)
           (iterate run-step)
           (drop-while #(some? (:steps %)))
           (first))
      dir (mod (dec dir) 4)]
  (println (+ (* 1000 (inc y)) (* 4 (inc x)) dir)))
