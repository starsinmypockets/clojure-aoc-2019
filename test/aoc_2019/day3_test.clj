(ns aoc-2019.day3-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day3 :refer :all :as aoc ]))

(deftest test-day-3
  (testing "Day 3"
    (let [inst-a ["R75","D30","R83","U83","L12","D49","R71","U7","L72"]
          inst-b ["U62","R66","U55","R34","D71","R55","D58","R83"]]
      (is (=(aoc/get-nearest-intersection inst-a inst-b) 159)))))

(deftest test-day-3-part-3
  (testing "Day 3 -- part 2"
    (let [inst-a ["R75","D30","R83","U83","L12","D49","R71","U7","L72"]
          inst-b ["U62","R66","U55","R34","D71","R55","D58","R83"]]
      (is (= (aoc/shortest-combined-path inst-a inst-b) 610))))
  
  (testing "Day 3 -- part 2"
    (let [inst-a ["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"]
          inst-b ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]]
      (is (= (aoc/shortest-combined-path inst-a inst-b) 410)))))
