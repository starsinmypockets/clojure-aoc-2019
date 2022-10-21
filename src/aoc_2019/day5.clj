(ns aoc-2019.day5)

;;
; TODO this is note finished
;;

(defn reverse-num [n]
  (Integer. (clojure.string/reverse (str n))))

(defn decode-command [n]
  (let [code (reverse-num n)]))
    ; ; zero-pad
    ; (if (< code 100)
    ;   (* code 1000)
    ;   (if (< code 1000)
    ;     (* code 1000)
    ;     (if (< code 10000)
    ;       (* code 10)
    ;       code)))))

(decode-command 12)
(decode-command 120)
(decode-command 2001)
(decode-command 21012)

(defn getValue [ll mode param]
  (if (= mode 0)
    (ll param)
    param))

(defn do-op [opcode params]
  (if (= opcode 2)
    ; the last vector value is the value
    (reduce * (map last params))
    (if (= opcode 1)
      (reduce + (map last params))
      (if (= opcode 3)
        1))))

(defn process-command-2 [ll i last-val]
  (let [code (reverse-num (ll i))
        opcode (Integer/parseInt (subs (str code) 0 1))]
    (if (= opcode 4)
      (ll (ll (+ i 1)))
      (let [; cmd (decode-command (ll i))
            p-count (- (count (vec (str code))) 1)
            params (map (fn [n]
                          (let [param (ll (+ i n 1)) ; param follows instruction from start
                                mode (Character/digit ((vec (str code)) (+ n 2)) 10)
                                value (getValue ll mode param)] ;; mode follows op-code at n
                            [param mode value])) (range 0 p-count))
            result (do-op opcode params)
            next-start (+ i (count params) 1)]
        (process-command-2 (assoc ll (last (last params)) result) next-start result)))))

