(ns aoc-2019.day6)

(defn read-file [f]
  (-> (slurp f)
     ( clojure.string/split-lines)))

(defn walk-orbits
  "Count steps to reach universal center of mass"
  [orbits body i]
  (if (= body "COM")
    i
    (let [cur (filter #(= body (get % 1)) orbits)
          nxt ((nth cur 0) 0)]
      (walk-orbits orbits nxt (inc i)))))

(defn get-orbits
  "Determine number of direct and indirect orbits for given input"
  [data]
  (let [orbits (map #(clojure.string/split %1 #"\)") data)
        orbiters (set (for [x orbits] (x 1)))
        sums (map #(walk-orbits orbits % 0) orbiters)]
    (reduce + sums)))

(defn day-6 []
  (let [inputs (read-file "src/inputs/day-5.txt")]
    (get-orbits inputs)))

;;
; Day 6 -- Part 2
;;

(defn trace-orbits
  "Count steps to reach universal center of mass"
  [orbits ll]
  (if (= ((nth ll 0) 0) "COM")
    ll
    (let [cur (filter #(= ((nth ll 0) 0) (nth %1 1)) orbits)
          nxt-ll (reverse (into cur ll))]
      nxt-ll
      (trace-orbits orbits nxt-ll))))

(def day-6-test-data-raw '["COM)B"
                           "B)C"
                           "C)D"
                           "D)E"
                           "E)F"
                           "B)G"
                           "G)H"
                           "D)I"
                           "E)J"
                           "J)K"
                           "K)L"])

(def day-6-data (map #(clojure.string/split %1 #"\)") day-6-test-data-raw))

(defn day-6-2 []
  (let [raw (read-file "src/inputs/day-5.txt")
        inputs (map #(clojure.string/split %1 #"\)") raw)
        you (filter #(= (%1 1) "YOU") inputs)
        san (filter #(= (%1 1) "SAN") inputs)
        orbit-a (trace-orbits inputs you)
        orbit-b (trace-orbits inputs san)
        uniq-a (- (count (drop-while #(>= (.indexOf orbit-b %) 0) orbit-a)) 1)
        uniq-b (- (count (drop-while #(>= (.indexOf orbit-a %) 0) orbit-b)) 1)]
    (+ uniq-a uniq-b)))
