(ns aoc-2019.day4)

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
  (let [xs (filter valid? (range low hi))]
    (count xs)))

(defn day-4 [] (crack-pass 357253 892942))

;;
; Part 2
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
  (let [xs (filter valid-2? (range low hi))]
    (count xs)))

(defn day-4-part-2 [] (crack-pass-2 357253 892942))
