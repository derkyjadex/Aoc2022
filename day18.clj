(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-cubes [line]
  (->> (str/split line #",")
       (mapv read-string)))

(defn step [{cubes :cubes n :n} [x y z :as cube]]
  (let [touching (->> [[(inc x) y z]
                       [(dec x) y z]
                       [x (inc y) z]
                       [x (dec y) z]
                       [x y (inc z)]
                       [x y (dec z)]]
                      (filter #(contains? cubes %))
                      (count))]
    {:cubes (conj cubes cube)
     :n (+ n 6 (* -2 touching))}))

(->> (slurp "day18-input.txt")
     (str/split-lines)
     (map read-cubes)
     (reduce step {:cubes #{} :n 0})
     (:n)
     (println))
