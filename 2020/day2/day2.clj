'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day2/input.txt")
   (str/split-lines)))

(defn in-between?
  "Check if num is in between"
  [min max num]
  (<= min num max))

;; Could use re-seq to count occurrences
(defn is-valid1?
  "Check if entry is valid password"
  [entry]
  (let [[policy char password] (str/split entry #" ")]
    (let [[policyMin policyMax] (str/split policy #"-")]
      (->> (frequencies password)
           (#(get % ((comp first seq) char) 0))
           (in-between? (Integer/parseInt policyMin) (Integer/parseInt policyMax))))))

(defn is-valid2?
  "Check if entry is valid password"
  [entry]
  (let [[policy _char password] (str/split entry #" ")]
    (let [[index1 index2] (str/split policy #"-")
          password-list (seq password)
          char ((comp first seq) _char)]
      (or (and (= char (nth password-list (dec (Integer/parseInt index1))))
               (not (= char (nth password-list (dec (Integer/parseInt index2))))))
          (and (= char (nth password-list (dec (Integer/parseInt index2))))
               (not (= char (nth password-list (dec (Integer/parseInt index1))))))))))

(defn part1 []
  (prn "day2 part1:" (->> input
                          (filter is-valid1?)
                          (count))))

(defn part2 []
  (prn "day2 part2:" (->> input
                          (filter is-valid2?)
                          (count))))

(part1)
(part2)

;; (defn valid-passwords-1 [s]
;;   (count
;;    (for [line (str/split-lines s)
;;          :let [[_ lowest highest [letter] password]
;;                (re-matches #"(\d+)-(\d+) (.): (.*)" line)
;;                password (frequencies password)
;;                lowest   (Integer/parseInt lowest)
;;                highest  (Integer/parseInt highest)]
;;          :when (<= lowest (get password letter 0) highest)]
;;      letter)))

;; (defn valid-passwords-2 [s]
;;   (count
;;    (for [line (str/split-lines s)
;;          :let [[_ i j [letter] password]
;;                (re-matches #"(\d+)-(\d+) (.): (.*)" line)
;;                i (dec (Integer/parseInt i))
;;                j (dec (Integer/parseInt j))]
;;          :when (not= (= (get password i) letter)
;;                      (= (get password j) letter))]
;;      letter)))

;; --------------------------------------------------------------------------------

;; (defn parse-input [row]
;;   (let [[low high letter pattern] (str/split row #":*\s|-")]
;;     {:low (Integer/parseInt low)
;;      :high (Integer/parseInt high)
;;      :letter (first letter)
;;      :password pattern}))

;; (defn valid-password? [{:keys [password letter low high]}]
;;   (<= low (count (filter #(= letter %) password)) high))

;; (defn puzzle1 [input]
;;   (->> (map parse-input input) (filter valid-password?) count))

;; (defn valid-password-2? [{:keys [password letter low high]}]
;;   (not= (= letter (nth password (dec low)))
;;         (= letter (nth password (dec high)))))

;; (defn puzzle2 [input]
;;   (->> (map parse-input input) (filter valid-password-2?) count));; 

;; --------------------------------------------------------------------------------

;; (def input (file-util/read-lines "2020/d02.txt"))

;; math-util/xor
;; (defn xor
;;   "Returns true if exclusive or satisfied over all inputs, else false."
;;   ([a b] (and (or a b)
;;               (not (and a b))))
;;   ([a b & more] (reduce xor (xor a b) more)))

;; (defn parse-line [s]
;;   (let [[_ min max c pwd] (re-find #"^(\d+)-(\d+) ([a-z]): (\w+)$" s)]
;;     [(first c) (read-string min) (read-string max) pwd]))

;; (defn valid? [[c min max pwd]]
;;   (let [freq (get (frequencies pwd) c 0)]
;;     (<= min freq max)))

;; (defn valid2? [[c p1 p2 pwd]]
;;   (let [c1 (nth pwd (dec p1))
;;         c2 (nth pwd (dec p2))]
;;     (math-util/xor (= c c1) (= c c2))))

;; (defn solve [input rule] (count (filter rule (map parse-line input))))

;; (defn part-1 [input] (solve input valid?))

;; (defn part-2 [input] (solve input valid2?))