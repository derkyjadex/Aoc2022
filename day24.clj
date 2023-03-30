(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def eg "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(defn init [input]
  (let [lines (->> input
                   (str/split-lines)
                   (drop 1)
                   (drop-last)
                   (map #(->> %
                              (char-array)
                              (drop 1)
                              (drop-last))))
        left (->> lines
                  (mapv (fn [line] (mapv #(= \< %) line))))
        right (->> lines
                   (mapv (fn [line] (mapv #(= \> %) line))))
        up (->> (range (count (first lines)))
                (mapv (fn [x] (->> (range (count lines))
                                   (mapv #(= (nth (nth lines %) x) \^))))))
        down (->> (range (count (first lines)))
                  (mapv (fn [x] (->> (range (count lines))
                                     (mapv #(= (nth (nth lines %) x) \v))))))
        end [(dec (count (first lines)))
             (count lines)]]
    {:left left
     :right right
     :up up
     :down down
     :end end}))

(defn clear? [storms x y t]
  (let [left (get (:left storms) y)
        right (get (:right storms) y)
        up (get (:up storms) x)
        down (get (:down storms) x)
        width (count left)
        height (count up)]
    (or (= [x y] [0 -1])
        (= [x y] (:end storms))
        (and (< -1 x width)
             (< -1 y height)
             (not (or (get left (mod (+ x t) width))
                      (get right (mod (- x t) width))
                      (get up (mod (+ y t) height))
                      (get down (mod (- y t) height))))))))

(defn run [storms]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [0 -1 0])]
    (let [[x y t] (peek queue)
          queue (pop queue)]
      (if (= [x y] (:end storms))
        t
        (->> [[0 -1] [1 0] [0 1] [-1 0] [0 0]]
             (reduce (fn [queue [dx dy]]
                       (if (clear? storms (+ x dx) (+ y dy) (inc t))
                         (conj queue [(+ x dx) (+ y dy) (inc t)])
                         queue))
                     queue)
             (recur))))))

(->> (slurp "day24-input.txt")
     (init)
     (run)
     (println))
