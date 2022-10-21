(ns aoc-2019.day4-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day4 :refer :all :as aoc ]))

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
