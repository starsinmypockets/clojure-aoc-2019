(ns aoc-2019.day8)
(require '[clojure.string :as str])

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
          nxt-ll (if (= 0 (count ll)) (list layer) (conj ll layer))]
      (build-matrix w h nxt-input nxt-ll))))

(comment
  ;; TODO add to tests
  (build-matrix 2 2 '(0 1 2 3 4 5 6 7 8 9 0 1) '()))

(defn get-day-8 [data w h]
  (let [input (map #(Character/digit % 10) (vec data))
        matrix (reverse (build-matrix w h input ()))
        freqs (for [x matrix] (frequencies (flatten x)))
        least-zeros (first (sort #(< (or (%1 0) -1) (or (%2 0) -1)) freqs))
        validation (* (get least-zeros 1) (get least-zeros 2))]
    (println least-zeros validation)
    validation))

(defn day-8 []
  (let [data (slurp "src/inputs/day-8.txt")]
    (get-day-8 data 25 6)))

;;
; Part 2
;;

(defn get-layer-value
  "Use recursion to map over matrix and get final pixel values"
  [w h matrix cur i]
  (if (= (count matrix) (+ i 1))
    (reverse cur)
    (let [c (flatten cur)
          nxt (flatten (nth matrix (+ i 1)))
          adjusted-flat (for [i (range (count c))] (if (.contains '(0,1) (nth c i)) (nth c i) (nth nxt i)))
          adjusted-layer (build-layer w h adjusted-flat)]
      (get-layer-value w h matrix adjusted-layer (inc i)))))

(defn graphix
  "VGA"
  [matrix]
  (for [line matrix] (str/join (into (newline) (map (fn [x] (cond (= x 1) "⬛️" (= x 0) "⬜️" :else " ")) (flatten line))))))

(defn get-day-8-2 [data w h]
  (let [input (map #(Character/digit % 10) (vec data))
        matrix (build-matrix w h input '())
        fixed (reverse matrix)] ; reverse the matrix I can't figure out what is wrong with build-matrix
    (get-layer-value w h fixed (nth fixed 0) 0)))

(comment (def test-data "0222112222120000")
         (graphix (get-day-8-2 test-data 2 2)))

(graphix (get-day-8-2  (slurp "src/inputs/day-8.txt") 25 6))

(defn day-8-2-a []
  (let [data (slurp "src/inputs/day-8.txt")]
    (graphix (get-day-8-2 data 25 6))))
