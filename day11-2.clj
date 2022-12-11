(require '[clojure.string :as str])
(require '[clojure.set])

(defn read-monkey [input]
  (let [[_ items op div t f] (str/split-lines input)
        [op1 op2 op3] (read-string (str "[" (subs op 19) "]"))
        div (read-string (subs div 21))
        t (read-string (subs t 29))
        f (read-string (subs f 30))]
    {:items (read-string (str "[" (subs items 18) "]"))
     :op (eval (list 'fn '[old] (list op2 op1 op3)))
     :div div
     :choose (fn [x] (if (= 0 (mod x div)) t f))
     :count 0}))

(defn run-monkey [monkeys div i]
  (let [{items :items op :op choose :choose} (nth monkeys i)]
    (reduce (fn [monkeys item]
              (let [item' (mod (op item) div)]
                (update monkeys (choose item')
                        update :items
                        conj item')))
            (update monkeys i
                    (fn [monkey]
                      (-> monkey
                          (assoc :items [])
                          (update :count + (count items)))))
            items)))

(defn run-round [state]
  (reduce (fn [{div :div :as state} i]
            (update state :monkeys run-monkey div i))
          state
          (range (count (:monkeys state)))))

(defn init [input]
  (let [monkeys (->> (str/split input #"\n\n")
                     (map read-monkey)
                     (vec))]
    {:monkeys monkeys
     :div (apply * (map :div monkeys))}))

(->> (init (slurp "day11-input.txt"))
     (iterate run-round)
     (drop 10000)
     (first)
     (:monkeys)
     (map :count)
     (sort)
     (take-last 2)
     (reduce *)
     (println))
