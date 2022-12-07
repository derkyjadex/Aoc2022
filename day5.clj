(require '[clojure.string :as str])
(require '[clojure.set])

(defn read-stacks [stacks]
  (let [lines (drop-last (str/split stacks #"\n"))
        num-stacks (/ (+ 1 (count (first lines))) 4)]
    (mapv (fn [i] (->> lines
                       (map #(nth % (+ 1 (* i 4))))
                       (drop-while #(= \space %))
                       (apply list)))
          (range num-stacks))))

(defn read-cmd [line]
  (let [[n src dst] (->> line
        (re-matches #"move (\d+) from (\d+) to (\d+)")
        (drop 1)
        (map read-string))]
    (repeat n (list (- src 1) (- dst 1)))))

(defn read-cmds [body]
  (->> (str/split body #"\n")
       (map read-cmd)
       (apply concat)))

(defn run-cmd [stacks [src dst]]
  (let [item (peek (get stacks src))]
    (update (update stacks src pop) dst conj item)))

(let [[stacks cmds] (str/split (slurp "day5-input.txt") #"\n\n")]
  (->> (read-cmds cmds)
       (reduce run-cmd (read-stacks stacks))
       (map peek)
       (apply str)
       (println)))
