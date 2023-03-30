(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def eg "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn read-valve [line]
  (let [[_ valve rate nxt]
        (re-matches
          #"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.+)" line)]
    {:name valve
     :rate (read-string rate)
     :next (set (str/split nxt #", "))}))

(defn init [input]
  (let [valves (->> input
                    (str/split-lines)
                    (map read-valve))]
    {:valves (->> valves
                  (filter #(pos? (:rate %)))
                  (map #(vector (:name %) (:rate %)))
                  (into {}))
     :tunnels (->> valves
                   (map #(vector (:name %) (:next %)))
                   (into {}))
     :pos "AA"
     :time 30
     :released 0}))

(defn dist
  ([state end]
   (dist (:tunnels state) (:pos state) end #{(:pos state)}))

  ([tunnels start end visited]
   (if (= start end)
     0
     (->> (set/difference (get tunnels start) visited)
          (map #(dist tunnels % end (conj visited %)))
          (filter some?)
          (map inc)
          (sort)
          (first)))))

(defn solve [state]
  (cond
    (neg? (:time state))
    0

    (zero? (:time state))
    (:released state)

    (empty? (:valves state))
    (:released state)

    :else
    (->> (:valves state)
         (filter #(pos? (second %)))
         ;(take 5)
         (map (fn [[valve rate]]
                (do ;(println "looking at" valve "from" (:pos state) "with" (:time state) "left")
                    ;(println (dist state valve))
                    (let [t (inc (dist state valve))
                          released (* rate (- (:time state) t))]
                      (-> state
                          (update :valves dissoc valve)
                          (assoc :pos valve)
                          (update :time - t)
                          (update :released + released))))))
         (sort-by :released)
         (reverse)
         (take 7)
         (map solve)
         (sort)
         (last))))

(->> (slurp "day16-input.txt")
     (init)
     (solve)
     (println)
     )
