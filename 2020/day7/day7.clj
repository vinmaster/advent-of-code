'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day7/input.txt")
   (str/split-lines)))

(defn parse-bag [s] (let [[_ qty name] (re-find #"(\d+)?\s?(\w+ \w+) bag[s]?" s)]
                      {:qty (when-not (nil? qty) (Integer/parseInt qty))
                       :name name}))

(defn parse-line [line]
  (let [[bag-str & inner-bags-str] (map second (re-seq #"((\d+)?\s?\w+ \w+ bag[s]?)" line))
        bag-name (get (parse-bag bag-str) :name)
        inner-bags (map parse-bag inner-bags-str)]
    {bag-name inner-bags}))

(def rules
  (->>
   input
   (map parse-line)
   (into (sorted-map))))

(defn includes? [list val] (contains? (set list) val))

(defn get-containing [rules target]
  (distinct (flatten (for [[bag inner-bags] (seq rules)]
                       (if (includes? (map #(get % :name) inner-bags) target)
                         (conj (get-containing rules bag) bag)
                         [])))))

(defn get-bag-count [rules target]
  (+ 1 (apply + (map (fn [bag]
                       (if (nil? (get bag :qty))
                         0
                         (* (get bag :qty)
                            (get-bag-count rules (get bag :name)))))
                     (get rules target)))))

(defn part1 []
  (prn "day7 part1:"
       (count (get-containing rules "shiny gold"))))

(defn part2 []
  (prn "day7 part2:"
       (- (get-bag-count rules "shiny gold") 1)))

(part1)
(part2)
