(ns aoc-2019.day12)

(defn compare-moons
  "Compare moons' positions and calc gravity"
  [moon1 moon2 k]
  (cond
    (< (moon1 k) (moon2 k)) 1
    (> (moon1 k) (moon2 k)) -1
    :else 0))

(defn calc-velocity
  "Given moon and neighbors calculate the gravity"
  [moon others velocity]
  (if (= (count others) 0)
    velocity
    (let [next-moon (first others)
          rest-moons (rest others)
          nxt-velocity {:x (+ (compare-moons moon next-moon :x) (velocity :x))
                        :y (+ (compare-moons moon next-moon :y) (velocity :y))
                        :z (+ (compare-moons moon next-moon :z) (velocity :z))}]

      (calc-velocity moon rest-moons nxt-velocity))))

(def moons-1 '({:x -1 :y 0 :z 2}
               {:x 2 :y -10 :z -7}
               {:x 4 :y -8 :z 8}
               {:x 3 :y 5 :z -1}))

(def initial-velocity {:x 0 :y 0 :z 0})
(calc-velocity (first moons-1) (rest moons-1) initial-velocity)

(defn step
  "Calculate the next step in the gravitational system"
  [moons n i]

  (if (= n i)
    moons
    (let [next-moons (map (fn [moon] (let [cur-pos (nth moon 0)
                                           cur-vel (nth moon 1)
                                           rest-moons (map #(nth % 0) (filter #(not= moon %) moons))
                                           velocity (calc-velocity cur-pos rest-moons cur-vel)
                                           position {:x (+ (cur-pos :x) (velocity :x))
                                                     :y (+ (cur-pos :y) (velocity :y))
                                                     :z (+ (cur-pos :z) (velocity :z))}] (list position velocity))) moons)]
      (step next-moons n (inc i)))))

; calculate absolute value
(defn abs
  "Calculate absolute value"
  [x]
  (if (< x 0)
    (- x)
    x))

(defn calc-total-energy
  [moons]
  (let [pp (map #(reduce + (map abs (vals (nth % 0)))) moons)
        vv (map #(reduce + (map abs (vals (nth % 1)))) moons)
        e (reduce + (map * pp vv))]
    e))

(calc-total-energy (step (map #(list % initial-velocity) moons-1) 10 0))

(def moons-2 '({:x -8 :y -10 :z 0}
               {:x 5 :y 5 :z 10}
               {:x 2 :y -7 :z 3}
               {:x 9 :y -8 :z -3}))
(+ 2 3 4 5)
(def moons-3 '({:x -19 :y -4 :z 2}
               {:x -9 :y 8 :z -16}
               {:x -4 :y 5 :z -11}
               {:x 1 :y 9 :z -13}))

(calc-total-energy (step (map #(list % initial-velocity) moons-2) 100 0))

(calc-total-energy (step (map #(list % initial-velocity) moons-3) 1000 0))

([1 2 3] 0)
(defn get-series
  [moons dim]
  (for [moon moons] (list ((nth moon 0) dim) ((nth moon 1) dim))))

(defn get-dimension
  [dim moons cmp n]
  (let [nxt-moons (step moons 1 0)
        series (get-series moons dim)]
    (if (and (> n 0) (= cmp series))
      n
      (recur dim nxt-moons cmp (inc n)))))

; get greatest common divisor
(defn gcd
  "Get greatest common divisor"
  [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn day-12-2
  [moons]
  (let [xn (get-dimension :x moons (get-series moons :x) 0)
        yn (get-dimension :y moons (get-series moons :y) 0)
        zn (get-dimension :z moons (get-series moons :z) 0)
        d (reduce gcd (list xn yn zn))]
    (reduce * (map #(/ % d) (list xn yn zn)))))

(def m-1 (map #(list % initial-velocity) moons-1))
(get-dimension :x m-1 (get-series m-1 :x) 0)
; (day-12-2 m-1)
