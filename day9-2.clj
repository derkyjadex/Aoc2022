(require '[clojure.string :as str])
(require '[clojure.set])

(defn parse-line [line]
  (repeat (read-string (subs line 2))
          (nth line 0)))

(defn move [[x y] d]
  (case d
    \U [x (dec y)]
    \D [x (inc y)]
    \L [(dec x) y]
    \R [(inc x) y]))

(defn follow [[hx hy] [tx ty]]
  (let [[dx dy] [(- hx tx) (- hy ty)]]
    (case [dx dy]
      [0 -2] [tx (dec ty)]
      [0 2] [tx (inc ty)]
      [-2 0] [(dec tx) ty]
      [2 0] [(inc tx) ty]

      ([-1 -2] [-2 -2] [-2 -1]) [(dec tx) (dec ty)]
      ([1 -2] [2 -2] [2 -1]) [(inc tx) (dec ty)]
      ([1 2] [2 2] [2 1]) [(inc tx) (inc ty)]
      ([-1 2] [-2 2] [-2 1]) [(dec tx) (inc ty)]

      [tx ty])))

(defn step [[rope visited] d]
  (let [rope' (reduce (fn [rope knot]
                        (conj rope (follow (last rope) knot)))
                      [(move (first rope) d)]
                      (drop 1 rope))
        visited' (conj visited (last rope'))]
    [rope' visited']))

(def init [(repeat 10 [0 0]) #{}])

(->> (slurp "day9-input.txt")
     (str/split-lines)
     (map parse-line)
     (flatten)
     (reduce step init)
     (second)
     (count)
     (println))
