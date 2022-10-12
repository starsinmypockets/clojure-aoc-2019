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
  (let [code (reverse-num (ll i))
        opcode (Integer/parseInt (subs (str code) 0 1))]
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

(def day-6-test-data-raw '[
  "COM)B"
  "B)C"
  "C)D"
  "D)E"
  "E)F"
  "B)G"
  "G)H"
  "D)I"
  "E)J"
  "J)K"
  "K)L"
])

(def day-6-data (map #(clojure.string/split %1 #"\)") day-6-test-data-raw) )

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

;;
; Day 8
;;

(defn build-layer
  "Build list of arrays of w * h dimension"
  [w h input]
  (map (fn [x] (take  w (drop (* w x) input))) (range h)))

(build-layer 2 2 '(0 2 1 2))

(defn build-matrix
  "returns list (ll) of lists (layers) corresponsding with w (layer x) and h (layer y)
  for example w = 3 h = 2
  (
    (
       ( 0 1 2 )
       ( 3 4 5 )
    )
    (  
      ( 6 7 8 )
      ( 9 0 1 )
    )
  )"
  [w h input ll]
    (if (< (count input) (* w h))
     ll
      (let [layer (build-layer w h input)
            nxt-input (drop (* w h) input)
            nxt-ll (if (= 0 (count ll)) (list layer) (conj ll layer))
            ]
        (build-matrix w h nxt-input nxt-ll))))

(build-matrix 2 2 '(0 1 2 3 4 5 6 7 8 9 0 1) '())

(defn get-day-8 [data w h]
  (let [input (map #(Character/digit % 10) (vec data))
        matrix (reverse (build-matrix w h input ()))
        freqs (for [x matrix] (frequencies (flatten x)))
        least-zeros (first (sort #(< (or (%1 0) -1) (or (%2 0) -1)) freqs ))
        validation (* (get least-zeros 1) (get least-zeros 2))]
    (println least-zeros validation)
    validation))

(defn day-8 []
  (let [data (slurp "src/inputs/day-8.txt")]
    (get-day-8 data 25 6)))

;;
; Day 8 -- Part 2
;;
(defn get-layer-value
  "Use recursion to map over matrix and get final pixel values"
  [w h matrix cur i]
      (if (= (count matrix) (+ i 1))
          (reverse cur)
          (let [c (flatten cur)
                nxt (flatten (nth matrix (+ i 1)))
                adjusted-flat (for [i (range (count c))] (if (.contains '(0,1) (nth c i)) (nth c i) (nth nxt i)))
                adjusted-layer (build-layer w h adjusted-flat )]
                (get-layer-value w h matrix adjusted-layer (inc i)))))

(defn graphix 
  "VGA"
  [matrix]
    (for [line matrix] (str/join (into (newline) (map (fn [x] (cond (= x 1) "⬛️" (= x 0) "⬜️" :else " ")) (flatten line))))))

(defn get-day-8-2 [data w h]
  (let [input (map #(Character/digit % 10) (vec data))
        matrix (build-matrix w h input '())
        fixed (reverse matrix)] ; reverse the matrix I can't figure out what is wrong with build-matrix
      (get-layer-value w h fixed (nth fixed 0) 0)
    )
  )

(def test-data "0222112222120000")
(graphix (get-day-8-2 test-data 2 2))
(graphix (get-day-8-2  (slurp "src/inputs/day-8.txt") 25 6))

(defn day-8-2-a []
  (let [data (slurp "src/inputs/day-8.txt")]
    (graphix (get-day-8-2 data 25 6))))

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
  (day-6-2)
  (day-8)
  (day-8-2-a)
)
