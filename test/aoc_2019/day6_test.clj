(ns aoc-2019.day6-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day6 :refer :all :as aoc ]))

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

