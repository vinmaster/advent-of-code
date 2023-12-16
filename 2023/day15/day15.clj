(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(def input
  (-> input-raw
      str/trim
      (str/split #",")))

(defn includes? [list val] (contains? (set list) val))

(defn hash-str->int [str]
  (reduce #(-> %1 (+ (int %2)) (* 17) (mod 256)) 0 (seq str)))

(defn perform-step [boxes step]
  (if (str/includes? step "=")
    (let [[label num] (str/split step #"=")
          num (parse-long num)
          label-hash (hash-str->int label)
          update-box (fn [box]
                       (if (includes? (map first box) label)
                         (mapv (fn [lens]
                                 (if (= (first lens) label)
                                   [label num]
                                   lens)) box)
                         (conj box [label num])))]
      (map-indexed (fn [i box] (if (= i label-hash) (update-box box) box)) boxes))
    (let [label (first (str/split step #"-"))
          label-hash (hash-str->int label)
          update-box (fn [box] (filterv #(not= (first %) label) box))]
      (map-indexed (fn [i box] (if (= i label-hash) (update-box box) box)) boxes))))

(defn focusing-power [i box]
  (->> box
       (map-indexed #(* (inc i) (inc %1) (second %2)))
       (reduce +)))

(defn part1 [input]
  (->> input
       (map hash-str->int)
       (reduce +)))

(defn part2 [input]
  (let [boxes (repeat 256 [])]
    (->> input
         (reduce perform-step boxes)
         (map-indexed focusing-power)
         (reduce +))))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
