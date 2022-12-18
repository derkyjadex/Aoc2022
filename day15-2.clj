(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn dist [ax ay bx by]
  (+ (Math/abs (- ax bx)) (Math/abs (- ay by))))

(defn pairs [items]
  (->> (iterate rest items)
       (drop 1)
       (take-while seq)
       (map #(map vector items %))
       (apply concat)))

(->> (slurp "day15-input.txt")
     (str/split-lines)
     (map #(->> %
                (re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
                (drop 1)
                (map read-string)))
     (map (fn [[sx sy bx by]]
            [sx sy (dist sx sy bx by)]))
     (pairs)
     (filter (fn [[[ax ay ad] [bx by bd]]] (= 2 (- (dist ax ay bx by) ad bd))))
     (map (fn [[[ax ay ad] [bx by bd]]]
            [(= (< ax bx) (< ay by))
             (set (map vector
                       (if (< ax bx)
                         (range ax (+ ax ad 2))
                         (range ax (- ax ad 2) -1))
                       (if (< ay by)
                         (range (+ ay ad 1) (- ay 1) -1)
                         (range (- ay ad 1) (+ ay 1)))))]))
     (pairs)
     (filter (fn [[[a] [b]]] (not= a b)))
     (map (fn [[[_ a] [_ b]]] (set/intersection a b)))
     (filter not-empty)
     (apply set/union)
     (map (fn [[x y]] (+ (* 4000000 x) y)))
     (first)
     (println))
