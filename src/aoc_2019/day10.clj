(ns aoc-2019.day10)

;;
; TODO haven't figured out how to do this yet
;;

(defn astr-map
  "Process grid into list of lists"
  [grid]
  (map #(vec %) grid))

(defn horizontal? [daby dacy] (and (= daby 0) (= dacy 0)))
(defn vertical? [dabx dacx] (and (= dabx 0) (= dacx 0)))

; determine quadrant of point b reletive to point a
(defn quadrant [dabx daby]
  (cond
    (and (> dabx 0) (> daby 0)) 1
    (and (< dabx 0) (> daby 0)) 2
    (and (< dabx 0) (< daby 0)) 3
    (and (> dabx 0) (< daby 0)) 4
    :else 0))

(defn eclipses 
  "determine if b is eclipsed by c"
  [a b c]
  (let [dabx (- (nth b 0) (nth a 0))
        daby (- (nth b 1) (nth a 1))
        dacx (- (nth c 0) (nth a 0))
        dacy (- (nth c 1) (nth a 1))
        qa (quadrant dabx daby)
        qb (quadrant dacx dacy)
        ]
    (cond
      (not= qa qb) false
      (horizontal? daby dacy) true
      (vertical? dabx dacx) true
      (= daby 0) false
      (= dacy 0) false
      (= (/ dabx daby) (/ dacx dacy)) true
      :else false
    )))


(defn count-visible
  "For each point i in coords count visible bodies n"
  [coords origin]
  (let [
        eclipsed (for [point-2 coords] (count (filter true? (map (fn [point-3] (eclipses origin point-3 point-2)) (filter #(or (not= origin %) (not= point-2 %)) coords)))))
        visible (count (filter #(= % 1) eclipsed))
        ] (println ["FOO" eclipsed]) visible))

(eclipses [2,1] [3,3] [4,4])
(eclipses [3,3] [2,2] [4,4])
(eclipses [2,2] [3,3] [4,4])

; 33 from 5, 8
(def day-10-a '(
  "......#.#.",
  "#..#.#....",
  "..#######.",
  ".#.#.###..",
  ".#..#.....",
  "..#....#.#",
  "#..#....#.",
  ".##.#..###",
  "##...#..#.",
  ".#....####"))

; 35 from 1,2
(def day-10-b '(
"#.#...#.#.",
".###....#.",
".#....#...",
"##.#.#.#.#",
"....#.#.#.",
".##..###.#",
"..#...##..",
"..##....##",
"......#...",
".####.###.",
  ))

; 210 from 11, 13
(def day-10-d '(
".#..##.###...#######",
"##.############..##.",
".#.######.########.#",
".###.#######.####.#.",
"#####.##.#.##.###.##",
"..#####..#.#########",
"####################",
"#.####....###.#.#.##",
"##.#################",
"#####.##.###..####..",
"..######..##.#######",
"####.##.####...##..#",
".#####..#.######.###",
"##...#.##########...",
"#.##########.#######",
".####.#.###.###.#.##",
"....##.##.###..#####",
".#.#.###########.###",
"#.#.#.#####.####.###",
"###.##.####.##.#..##",))

(defn get-coords
  "get list of coordinates of asteroids"
  [amap]
  (let [plot (map-indexed (fn [i x] (map-indexed (fn [j y] (and (= y \#) (list i,j))) x )) amap)
        coords (map (fn [row] (filter list? row)) plot)
        flat (reduce concat coords)]
    flat)
  )

(defn get-day-10
  []
  (let [coords (get-coords day-10-d)
        t (count-visible coords '(5 8))]
    (println coords)
  t))

(get-day-10)
