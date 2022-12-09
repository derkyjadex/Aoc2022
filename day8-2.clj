(require '[clojure.string :as str])
(require '[clojure.set])

(defn height [trees [x y]]
  (nth (nth trees y) x))

(defn view [height others]
  (first (reduce (fn [[n blocked] h]
                   (if blocked
                     [n true]
                     [(+ n 1) (>= h height)]))
                 [0 false]
                 others)))

(defn scan [trees dir x y]
  (->> (case dir
         :top (map vector
                   (repeat x)
                   (range y -1 -1))
         :bottom (map vector
                      (repeat x)
                      (range y (+ 0 (count trees)) 1))
         :left (map vector
                    (range x -1 -1)
                    (repeat y))
         :right (map vector
                     (range x (+ 0 (count (first trees))) 1)
                     (repeat y)))
       (drop 1)
       (map #(height trees %))
       (view (height trees [x y]))))

(def trees
  (->> (slurp "day8-input.txt")
       (str/split-lines)
       (map #(map read-string (str/split % #"")))))

(->> (for [x (range (count (first trees)))
           y (range (count trees))]
       (* (scan trees :top x y)
          (scan trees :bottom x y)
          (scan trees :left x y)
          (scan trees :right x y)))
     (apply max)
     (println))
