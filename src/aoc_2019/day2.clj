(ns aoc-2019.day2)
(require '[clojure.string :as str])

(defn process-command [ll start]
  (def command (get ll (+ start 0)))
  (def loc (get ll (+ start 3)))
  (def a (get ll (+ start 1)))
  (def b (get ll (+ start 2)))

  (if (= 1 command)
    (recur (assoc ll loc (+ (get ll a) (get ll b))) (+ start 4))
    (if (= 2 command)
      (recur (assoc ll loc (* (get ll a) (get ll b))) (+ start 4))
      (if (= 99 command) (get ll 0)
          (println "Error -- invalid case")))))

(defn day-2 []
  (def input (mapv #(Integer/parseInt (str/trim %)) (str/split (slurp "src/inputs/day-2.txt") #",")))
  (def target 19690720)
  (loop [noun 0]
    (when (<= noun 99)
      (loop [verb 0]
        (when (<= verb 99)
          (def multiply-ll (assoc input 1 noun 2 verb))
          (def multiply-output (process-command multiply-ll 0))
          (if (= target multiply-output)
            '[noun verb]
            (recur (+ verb 1)))))
      (if (= target multiply-output)
        noun
        (recur (+ noun 1))))))
