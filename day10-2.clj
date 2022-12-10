(require '[clojure.string :as str])
(require '[clojure.set])

(defn init [prog]
  {:prog (str/split-lines prog)
   :n 0
   :x 1
   :adding nil
   :screen []})

(defn update-screen [{n :n x :x :as cpu}]
  (update cpu :screen conj
          (if (contains? #{-1 0 1} (- x (mod n 40))) \# \space)))

(defn tick [{[cmd] :prog
             adding :adding
             :as cpu}]
  (if (not (nil? adding))
    (-> cpu
        (update-screen)
        (update :x + adding)
        (assoc :adding nil)
        (update :n inc)
        (update :prog rest))
    (cond
      (nil? cmd)
      cpu

      (= cmd "noop")
      (-> cpu
          (update-screen)
          (update :n inc)
          (update :prog rest))

      (str/starts-with? cmd "addx")
      (-> cpu
          (update-screen)
          (assoc :adding (read-string (subs cmd 4)))
          (update :n inc)))))


(->> (reduce (fn [cpu _] (tick cpu))
             (init (slurp "day10-input.txt"))
             (range 240))
     (:screen)
     (partition-all 40)
     (map #(apply str %))
     (str/join "\n")
     (println))
