(ns aoc-2019.core-test
  (:require [clojure.test :refer :all]
            [aoc-2019.core :refer :all :as aoc ]))

(deftest test-day-1
  (testing "Day 1"
    (is (= (aoc/day-1) 3563458)))
)

(deftest test-day-2
  (testing "Day 2"
    (is (= (aoc/day-2) 79)))
)

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

(deftest test-day-4
  (testing "Day 4"
    (and
      (is (false? (aoc/valid? 12321))
      (is (true? (aoc/valid? 122345)))))))

(deftest test-day-4-2  (testing "Day 4 -- part 2"
    (and
      (is (true?  (aoc/xpair? [1 1 1 4 5 5] 0)))
      (is (true?  (aoc/xpair? [1 1 2 4 5 6] 0)))
      (is (true?  (aoc/xpair? [1 1 2 2 3 3] 0)))
      (is (false? (aoc/xpair? [1 2 4 5 6 7] 0)))
      (is (false? (aoc/xpair? [1 2 4 4 4 6] 0)))
      (is (false? (aoc/xpair? [1 2 3 4 4 4] 0)))
      (is (false? (aoc/xpair? [3 5 7 7 7 7] 0)))
    )))

(deftest test-day-6
  (testing "Day 6"    
    (let [test-data-6 '[
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
    ]]
     (is (= 42 (aoc/get-orbits test-data-6))))))

(deftest test-day-8
  (testing "Day 8"
    (def test-data-8 "0222112222120000")
      (is (= (aoc/get-day-8 test-data-8 2 2) 4))))

(deftest test-day-8-2
  (testing "Day 8 -- part 2"
    (def test-data-8-2 "0222112222120000")
      (is (= (aoc/get-day-8-2 test-data-8-2 2 2) '((1 0)(0 1))))))
