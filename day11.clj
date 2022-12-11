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
     :choose (fn [x] (if (= 0 (mod x div)) t f))
     :count 0}))

(defn run-monkey [monkeys i]
  (let [{items :items op :op choose :choose} (nth monkeys i)]
    (reduce (fn [monkeys item]
              (let [item' (quot (op item) 3)]
                (update monkeys (choose item')
                        update :items
                        conj item')))
            (update monkeys i
                    (fn [monkey]
                      (-> monkey
                          (assoc :items [])
                          (update :count + (count items)))))
            items)))

(defn run-round [monkeys]
  (reduce run-monkey monkeys (range (count monkeys))))

(->> (str/split (slurp "day11-input.txt") #"\n\n")
     (map read-monkey)
     (vec)
     (iterate run-round)
     (drop 20)
     (first)
     (map :count)
     (sort)
     (take-last 2)
     (reduce *)
     (println))
