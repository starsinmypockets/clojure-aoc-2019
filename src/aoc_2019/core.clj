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
(defn digits [number] (vec (map #(Character/digit % 10) (str number))))
; (defn has-twin [ll] true)

(defn asc? [ll] 
  (let [asc-vals (map-indexed (fn [idx _itm] (if (< idx (- (count ll) 1)) (>= (nth ll (+ idx 1)) (nth ll idx)))) ll)]
    (= (count (filter true? asc-vals)) 5)))

(defn has-pair? [ll] 
  (let [asc-vals (map-indexed (fn [idx _itm] (if (< idx (- (count ll) 1)) (= (nth ll (+ idx 1)) (nth ll idx)))) ll)]
    (>= (count (filter true? asc-vals)) 1)))

(defn valid? [n]
  (let [ll (digits n)]
   (and (asc? ll) (has-pair? ll))))

(defn crack-pass [low hi]
  (let [ xs (filter valid? (range low hi))]
    (count xs)))

(defn day-4 [] (crack-pass 357253 892942))

;;
; Day 4 -- Part 2
;;

(defn xpair? [ll i]
  (if (>= i 5)
    false
    (if (= i 4)
      (if (= (ll 0) (ll 1))
        true
        false)
    (if (= (ll 0) (ll 1))
      (let [n (count (take-while #(= (ll 0) %) ll))]
        (if (= n 2)
          true 
          (xpair? (vec (drop n ll)) (+ i n))))
      (xpair? (vec (drop 1 ll)) (+ i 1))))))

(defn valid-2? [n]
  (let [ll (digits n)]
   (and (asc? ll) (xpair? ll 0))))

(defn crack-pass-2 [low hi]
  (let [ xs (filter valid-2? (range low hi))]
    (count xs)))

(defn day-4-part-2 [] (crack-pass-2 357253 892942))

;;
; Day 5
;;

(defn reverse-num [n]
  (Integer. (clojure.string/reverse (str n))))

(defn decode-command [n] 
  (let [code (reverse-num n)]))
    ; ; zero-pad
    ; (if (< code 100)
    ;   (* code 1000)
    ;   (if (< code 1000)
    ;     (* code 1000)
    ;     (if (< code 10000)
    ;       (* code 10)
    ;       code)))))

(decode-command 12)
(decode-command 120)
(decode-command 2001)
(decode-command 21012)

(defn getValue [ll mode param] 
  (if (= mode 0)
    (ll param)
    param))

(defn do-op [opcode params]
  (if (= opcode 2)
    ; the last vector value is the value
    (reduce * (map last params))
    (if (= opcode 1)
      (reduce + (map last params))
      (if (= opcode 3)
        1))))

(defn process-command-2 [ll i last-val]
  (println ll i last-val)
  (let [code (reverse-num (ll i))
        opcode (Integer/parseInt (subs (str code) 0 1))]
    (println [code opcode])
    (if (= opcode 4)
      (ll (ll (+ i 1)))
      (let [
            ; cmd (decode-command (ll i))
            p-count (- (count (vec (str code))) 1)
            params (map (fn [n]
                    (let [param (ll (+ i n 1)) ; param follows instruction from start
                          mode (Character/digit ((vec (str code)) (+ n 2)) 10)
                          value (getValue ll mode param)] ;; mode follows op-code at n
                      [param mode value])) (range 0 p-count))
            result (do-op opcode params)
            next-start (+ i (count params) 1)]
	    (println [(last (last params)) p-count params])
	    (process-command-2 (assoc ll (last (last params)) result) next-start result)))))

;;
; Day 6
;;

(defn walk-orbits
  "Count steps to reach universal center of mass"
  [orbits body i]
  (if (= body "COM")
    i
    (let [cur (filter #(= body (get % 1)) orbits)
          nxt ((nth cur 0) 0)]
      (walk-orbits orbits nxt (inc i))
      )))

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

(defn -main
  "Advent of Code -- 2019 https://adventofcode.com/2019/"
  []
  (day-1)
  (int (day-1b))
  (day-2)
  (day-3)
  (day-3-part-2)
  (day-4)
  (day-4-part-2)
  (day-6)
)
