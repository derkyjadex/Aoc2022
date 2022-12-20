(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def rocks [[[0 0] [1 0] [2 0] [3 0]]
            [[1 0] [0 1] [1 1] [2 1] [1 2]]
            [[0 0] [1 0] [2 0] [2 1] [2 2]]
            [[0 0] [0 1] [0 2] [0 3]]
            [[0 0] [1 0] [0 1] [1 1]]])

(defn move [rock dx dy]
  (map #(vector (+ dx (first %)) (+ dy (second %))) rock))

(defn init [input]
  {:jets (->> input
              (char-array)
              (seq)
              (keep #(case % \< -1 \> 1 nil))
              (cycle))
   :rocks (drop 1 (cycle rocks))
   :rock (move (first rocks) 2 4)
   :floor (->> (range 0 7)
               (map #(vector % 0))
               (set))
   :count 0})

(defn step [{[jet & jets] :jets
             rocks :rocks
             rock :rock
             floor :floor
             count :count
             :as state}]
  (let [rock' (move rock jet 0)
        rock' (if (every? #(and (<= 0 (first %) 6)
                                (not (contains? floor %)))
                          rock')
                rock'
                rock)
        rock'' (move rock' 0 -1)
        hit-floor? (some #(contains? floor %) rock'')
        floor' (if hit-floor?
                 (apply conj floor rock')
                 floor)
        rock'' (if hit-floor?
                 (let [top (apply max (map second floor'))]
                   (move (first rocks) 2 (+ 4 top)))
                 rock'')
        rocks' (if hit-floor? (rest rocks) rocks)
        count' (if hit-floor? (inc count) count)
        ]
    (assoc state
           :jets jets
           :rocks rocks'
           :rock rock''
           :floor floor'
           :count count')))

(->> (slurp "day17-input.txt")
     (init)
     (iterate step)
     (drop-while #(< (:count %) 2022))
     (map #(apply max (map second (:floor %))))
     (first)
     (println))
