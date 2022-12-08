(require '[clojure.string :as str])
(require '[clojure.set])

(def init-state {:paths ["/"] :sizes {}})

(defn add-file [state size]
  (reduce (fn [state path]
            (update state :sizes update path #(+ (or % 0) size)))
          state (:paths state)))

(defn process [state line]
  (cond
    (= line "$ cd /")
    (assoc state :paths ["/"])

    (= line "$ cd ..")
    (update state :paths pop)

    (str/starts-with? line "$ cd ")
    (update state :paths conj (str (peek (:paths state)) (subs line 5) "/"))

    (re-matches #"^(\d+) .+$" line)
    (add-file state (read-string line))

    :else state))

(->> (slurp "day7-input.txt")
     (str/split-lines)
     (reduce process init-state)
     (:sizes)
     (map second)
     (filter #(<= % 100000))
     (reduce +)
     (println))
