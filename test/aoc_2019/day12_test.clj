(ns aoc-2019.day12-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day12 :refer :all :as aoc]))

(deftest test-day-12
  (testing "Day 12 -- test progressions"
    (let [initial-velocity {:x 0 :y 0 :z 0}
          moons '({:x -1 :y 0 :z 2}
                  {:x 2 :y -10 :z -7}
                  {:x 4 :y -8 :z 8}
                  {:x 3 :y 5 :z -1})
          expected '(({:x 2 :y 1 :z -3}, {:x -3 :y -2 :z 1})
                     ({:x 1 :y -8 :z 0}, {:x -1 :y 1 :z 3})
                     ({:x 3 :y -6 :z 1}, {:x 3 :y 2 :z -3})
                     ({:x 2 :y 0 :z 4}, {:x 1 :y -1 :z -1}))
          actual (step (map #(list % initial-velocity) moons) 10 0)]
      (is (= expected actual)))))

(deftest test-day-12-2
  (testing "Day 12 - 2 -- expect get dimension to return appropriate value"
    (let [initial-velocity {:x 0 :y 0 :z 0}
          moons '({:x -1 :y 0 :z 2}
                  {:x 2 :y -10 :z -7}
                  {:x 4 :y -8 :z 8}
                  {:x 3 :y 5 :z -1})
          m1 (map #(list % initial-velocity) moons)
          actual (get-dimension :x m1 (get-series m1 :x) 0)]
      (is (= actual 18))))
  (testing "Day 12 - 2 -- expect day-12-2 to return correct value"
    (let [initial-velocity {:x 0 :y 0 :z 0}
          moons '({:x -1 :y 0 :z 2}
                  {:x 2 :y -10 :z -7}
                  {:x 4 :y -8 :z 8}
                  {:x 3 :y 5 :z -1})
          m1 (map #(list % initial-velocity) moons)
          actual (day-12-2 m1)]
      (is (= actual 2772)))))
