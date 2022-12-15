(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn init [input]
  (->> (str/split input #"\n\n")
       (map #(map read-string (str/split-lines %)))))

(defn cmp [[l & lrest :as left] [r & rrest :as right]]
  (cond
    (and (nil? l) (nil? r))
    0

    (nil? l)
    -1

    (nil? r)
    1

    (and (instance? Long l) (instance? Long r))
    (case (compare l r)
      -1 -1
      0 (cmp lrest rrest)
      1 1)

    (instance? Long l)
    (cmp (cons [l] lrest) right)

    (instance? Long r)
    (cmp left (cons [r] rrest))

    :else
    (case (cmp l r)
      -1 -1
      0 (cmp lrest rrest)
      1 1)))

(->> (slurp "day13-input.txt")
     (init)
     (map-indexed (fn [i [l r]] (if (neg? (cmp l r)) (inc i) 0)))
     (filter pos?)
     (reduce +)
     (println))
