(ns aoc-2019.day1-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day1 :refer :all :as aoc ]))

(deftest test-day-1
  (testing "Day 1"
    (is (= (aoc/day-1) 3563458)))
)
