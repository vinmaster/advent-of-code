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

;; (ns adventofcode.2020.day7
;;   (:require
;;    [clojure.string :as string]))

;; (def input
;;   (string/split-lines (slurp "resources/2020/day7.txt")))

;; (defn parse [line]
;;   (let [[bag-type & contents] (-> line
;;                                   (string/replace #" bags?\.?" "")
;;                                   (string/split #" contain |, "))]
;;     {:amount 1
;;      :bag-type bag-type
;;      :contents (for [content (remove #{"no other"} contents)]
;;                  {:amount (Integer. (subs content 0 1))
;;                   :bag-type (first (re-find #"(\s?\w)+" (subs content 2)))})}))

;; (def rules
;;   (map parse input))

;; (defn contains-bag? [bag-type {:keys [contents] :as _rule}]
;;   (some (comp #{bag-type} :bag-type) contents))

;; (defn find-bags [bag-type]
;;   {:bag-type bag-type
;;    :paths (for [rule rules
;;                 :when (contains-bag? bag-type rule)]
;;             (find-bags (:bag-type rule)))})


;; ;; Part 1

;; (->> (tree-seq seq :paths (find-bags "shiny gold"))
;;      (keep :bag-type)
;;      set
;;      count
;;      dec)

;; ;; => 205


;; ;; Part 2

;; (defn get-rule [bag-type]
;;   (first (filter (comp #{bag-type} :bag-type) rules)))

;; (defn bags-count [{:keys [bag-type amount]}]
;;   (* amount (apply + 1 (->> bag-type get-rule :contents (map bags-count)))))

;; (dec (bags-count {:bag-type "shiny gold" :amount 1}))

;; ;; => 80902
