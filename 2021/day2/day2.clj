(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2021/day2/input.txt")
   (str/split-lines)
   (map (fn [x]
          (-> x
              (str/split #" ")
              (update-in [1] #(Integer/parseInt %)))))))

(defn part1 [xs]
  (let [run-command (fn [result [dir num]]
                      (case dir
                        "forward" (update result :x #(+ % num))
                        "down" (update result :y #(+ % num))
                        "up" (update result :y #(- % num))))]
    (->> xs
         (reduce run-command {:x 0 :y 0})
         ((juxt :x :y))
         (apply list)
         (apply *))))

(defn part2 [xs]
  (let [run-command (fn [result [dir num]]
                      (case dir
                        "forward" (-> result
                                      (update :x #(+ % num)) 
                                      (update :y #(+ % (* num (:aim result)))))
                        "down" (update result :aim #(+ % num))
                        "up" (update result :aim #(- % num))))]
    (->> xs
         (reduce run-command {:x 0 :y 0 :aim 0})
         ((juxt :x :y))
         (apply list)
         (apply *))))

(println "day2 part1:" (part1 input))
(println "day2 part2:" (part2 input))
