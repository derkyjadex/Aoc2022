(require '[clojure.string :as str])
(require '[clojure.set :as set])

(let [y 2000000
      sensors (->> (slurp "day15-input.txt")
                   (str/split-lines)
                   (map #(->> %
                              (re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
                              (drop 1)
                              (map read-string))))
      empties (->> sensors
                   (map (fn [[sx sy bx by]]
                          (let [d (+ (Math/abs (- sx bx)) (Math/abs (- sy by)))
                                r (- d (Math/abs (- sy y))) ]
                            (range (- sx r) (+ sx r 1)))))
                   (flatten)
                   (set))]
  (->> sensors
       (filter #(= y (nth % 3)))
       (map #(nth % 2))
       (set)
       (set/difference empties)
       (count)
       (println)))
