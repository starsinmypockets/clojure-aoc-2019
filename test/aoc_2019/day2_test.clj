(ns aoc-2019.day2-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day2 :refer :all :as aoc ]))

(deftest test-day-2
  (testing "Day 2"
    (is (= (aoc/day-2) 79)))
)
