(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def eg "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn read-blueprint [line]
  (let [[id ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian] (->> line
       (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.")
       (drop 1)
       (map read-string))]
    {:id id
     :ore {:ore ore-ore}
     :clay {:ore clay-ore}
     :obsidian {:ore obsidian-ore :clay obsidian-clay}
     :geode {:ore geode-ore :obsidian geode-obsidian}}))

(defn init [blueprint]
  {:blueprint blueprint
   :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
   :collected {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :time 0})

(defn can-build? [type {blueprint :blueprint collected :collected :as state}]
  (every? (fn [[t req]] (>= (t collected) req))
          (type blueprint)))

(defn build-robot [type state]
  (do ;(println "building" type "at" (:time state))
      (-> state
          (update :robots update type inc)
          (update :collected
                  (fn [collected]
                    (reduce (fn [c [type req]] (update c type - req))
                            collected
                            (type (:blueprint state))))))))

(defn collect-resources [{robots :robots :as state}]
  (update state :collected
          (fn [collected]
            (reduce (fn [c [t n]] (update c t + n))
                    collected
                    (:robots state)))))

(defn run-blueprint [target
                     {robots :robots
                      collected :collected
                      :as state}]
  (if (>= (:time state) 24)
    (do ;(println (:robots state) (:collected state) target)
        (:geode (:collected state)))
    (let [state' (-> state
                     (update :time inc)
                     (collect-resources))]
      (if (can-build? target state)
        (let [state'' (build-robot target state')]
          (max (run-blueprint :ore state'')
               (run-blueprint :clay state'')
               (run-blueprint :obsidian state'')
               (run-blueprint :geode state'')))
        (run-blueprint target state')))))

(defn f [state]
  (max (run-blueprint :ore state)
       (run-blueprint :clay state)))

(->> eg;(slurp "day19-input.txt")
     (str/split-lines)
     (map read-blueprint)
     (second)
     (init)
     (f)
     (println)
     )
