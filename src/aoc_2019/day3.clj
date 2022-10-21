(ns aoc-2019.day3)
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn manhattan-distance [p q]
  (+ (Math/abs (- (nth p 0) (nth q 0))) (Math/abs (- (nth p 1) (nth q 1)))))

(defn plot-point [cmd n prev]
  (case cmd
    \R (map + (list n 0) prev)
    \L (map + (list (- n) 0) prev)
    \U (map + (list 0 n) prev)
    \D (map + (list 0 (- n)) prev)))

(defn plot-points [inst prev]
  (let [cmd (first inst)
        n (second inst)]
    (map #(plot-point cmd (+ 1 %1) prev) (range n))))

(defn decode [code]
  (list (first code) (Integer/parseInt (str/join (rest code)))))

(defn plot-path [i, ii, nn]
  (if (< i (count ii))
    (let [prev (if (= i 0) '(0,0) (last nn))
          inst (decode (nth ii i))
          nexVec (into [] (concat nn (plot-points inst prev)))]
      (recur (inc i) ii nexVec))
    nn))

(defn get-nearest-intersection [inst-a inst-b]
  (let [list-a (plot-path 0 inst-a [])
        list-b (plot-path 0 inst-b [])
        xs (set/intersection (set list-a) (set list-b))]
    (apply min (map #(manhattan-distance %1 [0 0]) xs))))

(defn day-3 []
  (def input (str/split (slurp "src/inputs/day-3.txt") #"\n"))
  (def a (str/split (first input) #","))
  (def b (str/split (second input) #","))
  (get-nearest-intersection a b))

;;
; Part 2
;;

; cribbed this from SO
; scarcely believe it's not in the standard lib
(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn shortest-combined-path [inst-a inst-b]
  (let [list-a (plot-path 0 inst-a [])
        list-b (plot-path 0 inst-b [])
        xs (set/intersection (set list-a) (set list-b))]
    (apply min (for [x (seq xs)] (+ 2 (index-of (vec x) list-a) (index-of (vec x) list-b))))))

(defn day-3-part-2 []
  (let [input (str/split (slurp "src/inputs/day-3.txt") #"\n")
        a (str/split (first input) #",")
        b (str/split (second input) #",")]
    (shortest-combined-path a b)))
