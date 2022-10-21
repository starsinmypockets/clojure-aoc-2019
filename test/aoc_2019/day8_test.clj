(ns aoc-2019.day8-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day8 :refer :all :as aoc ]))

(deftest test-day-8
  (testing "Day 8"
    (def test-data-8 "0222112222120000")
      (is (= (aoc/get-day-8 test-data-8 2 2) 4))))

(deftest test-day-8-2
  (testing "Day 8 -- part 2"
    (def test-data-8-2 "0222112222120000")
      (is (= (aoc/get-day-8-2 test-data-8-2 2 2) '((1 0)(0 1))))))
