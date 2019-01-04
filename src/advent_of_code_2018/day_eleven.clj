(ns advent-of-code-2018.day-eleven
  (:require [clojure.math.combinatorics :as combo]
            [clojure.stacktrace :as stack]))

(def grid-size 300)

(defn get-hundreds-digit
  [number]
  (quot (mod number 1000) 100))

(defn calc-power-level
  [serial-number point]
  (let [x (first point)
        y (last point)
        rack-id (+ 10 x)
        beginning-power-level (* rack-id (+ serial-number (* rack-id y)))]
    (if (< beginning-power-level 100)
      -5
      (- (get-hundreds-digit beginning-power-level) 5))))

(defn gen-n-by-n
  [point n]
  (let [x (first point)
        y (last point)]
    (if (or (> x (- 301 n)) (> y (- 301 n)))
      nil
      (for [i (range x (+ x n))
            j (range y (+ y n))]
        [i j]))))

(defn score-n-by-n
  [point serial-number n]
  (let [three-by-three (gen-n-by-n point n)]
    (if (nil? three-by-three)
      -9223372036854775808
      (reduce + (map (partial calc-power-level serial-number) three-by-three)))))

(defn find-best-n-by-n
  [point serial-number]
  (let [scores (for [n (range 1 301)]
    [(score-n-by-n point serial-number n) n])]
     (last (sort-by first scores))))

(defn find-top-nine
  [serial-number]
  (let [points (for [x (range 1 (+ grid-size 1))
                     y (range 1 (+ grid-size 1))]
                 [x y])
        max-point (atom nil)
        max-power (atom -9223372036854775808)
        max-n     (atom nil)]
    (loop [pts points]
      (print (first pts))
      (let [nxt-point (first pts)]
        (if (nil? nxt-point)
          [max-point max-n max-power]
          (let [best-n-for-point (find-best-n-by-n nxt-point serial-number)
                n (last best-n-for-point)
                power (first best-n-for-point)]
            (do
              (if (> power @max-power)
                (do (reset! max-point nxt-point)
                    (reset! max-power power)
                    (reset! max-n n)))
              (recur (rest pts)))))))))

