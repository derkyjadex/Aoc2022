(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def face-moves
  {:l [[[-1 +1  0 :d] [ 0 +1  0] :u]
       [[-1  0 -1 :b] [ 0  0 -1] :f]
       [[-1 -1  0 :u] [ 0 -1  0] :d]
       [[-1  0 +1 :f] [ 0  0 +1] :b]]
   :r [[[+1 +1  0 :d] [ 0 +1  0] :u]
       [[+1  0 +1 :f] [ 0  0 +1] :b]
       [[+1 -1  0 :u] [ 0 -1  0] :d]
       [[+1  0 -1 :b] [ 0  0 -1] :f]]
   :u [[[ 0 +1 +1 :f] [ 0  0 +1] :b]
       [[+1 +1  0 :l] [+1  0  0] :r]
       [[ 0 +1 -1 :b] [ 0  0 -1] :f]
       [[-1 +1  0 :r] [-1  0  0] :l]]
   :d [[[ 0 -1 -1 :b] [ 0  0 -1] :f]
       [[+1 -1  0 :l] [+1  0  0] :r]
       [[ 0 -1 +1 :f] [ 0  0 +1] :b]
       [[-1 -1  0 :r] [-1  0  0] :l]]
   :f [[[ 0 +1 -1 :d] [ 0 +1  0] :u]
       [[+1  0 -1 :l] [+1  0  0] :r]
       [[ 0 -1 -1 :u] [ 0 -1  0] :d]
       [[-1  0 -1 :r] [-1  0  0] :l]]
   :b [[[ 0 +1 +1 :d] [ 0 +1  0] :u]
       [[-1  0 +1 :r] [-1  0  0] :l]
       [[ 0 -1 +1 :u] [ 0 -1  0] :d]
       [[+1  0 +1 :l] [+1  0  0] :r]]})

(defn adjacent-faces [cubes [[x y z] d]]
  (map (fn [[[x1 y1 z1 d1] [x2 y2 z2] d3]]
         (let [c1 [(+ x x1) (+ y y1) (+ z z1)]
               c2 [(+ x x2) (+ y y2) (+ z z2)]]
           (cond
             (contains? cubes c1) [c1 d1]
             (contains? cubes c2) [c2 d]
             :else [[x y z] d3])))
       (d face-moves)))

(defn solve [input]
  (let [cubes (->> input
                   (str/split-lines)
                   (map #(mapv read-string (str/split % #",")))
                   (set))
        start [(first (sort cubes)) :l]]
    (loop [faces #{start}
           queue (conj (clojure.lang.PersistentQueue/EMPTY) start)]
      (if (seq queue)
        (let [new-faces (->> (peek queue)
                             (adjacent-faces cubes)
                             (filter #(not (contains? faces %))))]
          (recur (apply conj faces new-faces)
                 (apply conj (pop queue) new-faces)))
        faces))))

(->> (slurp "day18-input.txt")
     (solve)
     (count)
     (println))
