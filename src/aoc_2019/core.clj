(ns aoc-2019.core
  (:gen-class)
   (:require [clojure.string :as str])
   (:require [clojure.set :as set]))

(defn read-file [f]
  (-> (slurp f)
     ( clojure.string/split-lines)))

;;
; Day 1
;;

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
    (println (day-1b-get-fuel 1969 []))
    (reduce + (mapv #(day-1b-get-fuel % []) data))))

;;
; Day 2
;;

(defn process-command [ll start]
  (def command (get ll (+ start 0)))
  (def loc (get ll (+ start 3)))
  (def a (get ll (+ start 1)))
  (def b (get ll (+ start 2)))

  (if (= 1 command) 
    (recur (assoc ll loc (+ (get ll a) (get ll b))) (+ start 4))
    (if (= 2 command) 
      (recur (assoc ll loc (* (get ll a) (get ll b))) (+ start 4))
      (if (= 99 command) (get ll 0)
        (println "Error -- invalid case")))))

(defn day-2 []
  (def input (mapv #(Integer/parseInt (str/trim %) ) (str/split (slurp "src/inputs/day-2.txt") #",")))
  (def target 19690720)
    (loop [noun 0]
      (when (<= noun 99)
        (loop [verb 0]
          (when (<= verb 99)
            (def multiply-ll (assoc input 1 noun 2 verb))
            (def multiply-output (process-command multiply-ll 0))
              (if (= target multiply-output)
                '[noun verb]
                (recur (+ verb 1)))))
         (if (= target multiply-output)
          noun
          (recur (+ noun 1)))))
  )

;;
; Day 3
;;

(defn manhattan-distance [p q]
  (+(Math/abs(- (nth p 0) (nth q 0))) (Math/abs(- (nth p 1) (nth q 1)))))

(defn plot-point [cmd n prev]
  (case cmd
    \R (map + (list n 0) prev)
    \L (map + (list (- n) 0) prev)
    \U (map + (list 0 n) prev)
    \D (map + (list 0 (- n)) prev)
  ))

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
      (recur (inc i) ii nexVec ))
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
; Day 3 - Part 2
;;

; cribbed this from SO
; scarcely believe it's not in the standard lib
(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn shortest-combined-path [inst-a inst-b]
  (let [list-a (plot-path 0 inst-a [])
        list-b (plot-path 0 inst-b [])
        xs (set/intersection (set list-a) (set list-b))]
    (apply min (for [x (seq xs)] (+ 2 (index-of (vec x) list-a) (index-of (vec x) list-b) )))))

(defn day-3-part-2 []
  (let [input (str/split (slurp "src/inputs/day-3.txt") #"\n")
        a (str/split (first input) #",")
        b (str/split (second input) #",")]
    (shortest-combined-path a b)))

;;
; Day 4
;;

; SO
(defn digits [number] (map #(Character/digit % 10) (str number)))
; (defn has-twin [ll] true)

(defn asc [ll] 
  (let [asc-vals (map-indexed (fn [idx _itm] (if (< idx (- (count ll) 1)) (>= (nth ll (+ idx 1)) (nth ll idx)))) ll)]
    (= (count (filter true? asc-vals)) 5)))

(defn has-pair [ll] 
  (let [asc-vals (map-indexed (fn [idx _itm] (if (< idx (- (count ll) 1)) (= (nth ll (+ idx 1)) (nth ll idx)))) ll)]
    (>= (count (filter true? asc-vals)) 1)))

(defn valid? [n]
  (let [ll (digits n)]
   (and (asc ll) (has-pair ll))))

(defn crack-pass [low hi]
  (let [ xs (filter valid? (range low hi))]
    (count xs)))

(defn day-4 [] (crack-pass 357253 892942))

(defn -main
  "Advent of Code -- 2019 https://adventofcode.com/2019/"
  []
  (day-1)
  (int (day-1b))
  (day-2)
  (day-3)
  (day-3-part-2)
  (day-4)
)
