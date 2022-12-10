(require '[clojure.string :as str])
(require '[clojure.set])

(defn init [prog]
  {:prog (str/split-lines prog)
   :n 1
   :x 1
   :adding nil
   :signal 0})

(defn update-signal [{n :n x :x :as cpu}]
  (if (= 20 (mod n 40))
    (update cpu :signal + (* n x))
    cpu))

(defn tick [{[cmd] :prog
             adding :adding
             :as cpu}]
  (if (not (nil? adding))
    (-> cpu
        (update-signal)
        (update :x + adding)
        (assoc :adding nil)
        (update :n inc)
        (update :prog rest))
    (cond
      (nil? cmd)
      cpu

      (= cmd "noop")
      (-> cpu
          (update-signal)
          (update :n inc)
          (update :prog rest))

      (str/starts-with? cmd "addx")
      (-> cpu
          (update-signal)
          (assoc :adding (read-string (subs cmd 4)))
          (update :n inc)))))


(->> (reduce (fn [cpu _] (tick cpu))
             (init (slurp "day10-input.txt"))
             (range 220))
     (:signal)
     (println))
