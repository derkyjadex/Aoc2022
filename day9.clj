(require '[clojure.string :as str])
(require '[clojure.set])

(defn parse-line [line]
  (repeat (read-string (subs line 2))
          (nth line 0)))

(defn step [[x y] d]
  (case d
    \U [x (dec y)]
    \D [x (inc y)]
    \L [(dec x) y]
    \R [(inc x) y]))

(def init {:head [0 0] :tail [0 0] :visited #{}})

(defn move [{head :head
             [tx ty :as tail] :tail
             visited :visited}
            d]
  (let [[hx hy :as head'] (step head d)
        [dx dy] [(- hx tx) (- hy ty)]
        tail' (cond
                (and (< (Math/abs dx) 2)
                     (< (Math/abs dy) 2)) tail
                (or (= dx 0)
                    (= dy 0)) (step tail d)
                :else head)
        visited (conj visited tail')]
    {:head head' :tail tail' :visited visited}))

(->> (slurp "day9-input.txt")
     (str/split-lines)
     (map parse-line)
     (flatten)
     (reduce move init)
     (:visited)
     (count)
     (println))
