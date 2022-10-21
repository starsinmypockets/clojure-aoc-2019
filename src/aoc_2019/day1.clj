(ns aoc-2019.day1)

(defn read-file [f]
  (-> (slurp f)
      (clojure.string/split-lines)))

(defn day-1-get-fuel [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn day-1 []
  (let [data (mapv #(Integer/parseInt %) (read-file "src/inputs/day-1.txt"))]
    (int (reduce + (mapv day-1-get-fuel data)))))

(defn day-1b-get-fuel [mass ll]
  (let [fuel (day-1-get-fuel mass)]
    (if (> fuel 0)
      (day-1b-get-fuel fuel (cons fuel ll))
      (reduce + ll))))

(defn day-1b []
  (let [data (mapv #(Integer/parseInt %) (read-file "src/inputs/day-1.txt"))]
    (reduce + (mapv #(day-1b-get-fuel % []) data))))
