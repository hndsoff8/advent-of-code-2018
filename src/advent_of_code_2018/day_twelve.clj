(ns advent-of-code-2018.day-twelve
  (:require [clojure.stacktrace :as stack]
            [clojure.math.combinatorics :as combo]
            [taoensso.timbre :as timbre]
            [clojure.string :as cs]))

(def small-input "#..#.#..##......###...###")
(def small-rules {"...##" "#",
                  "..#.." "#",
                  ".#..." "#",
                  ".#.#." "#",
                  ".#.##" "#",
                  ".##.." "#",
                  ".####" "#",
                  "#.#.#" "#",
                  "#.###" "#",
                  "##.#." "#",
                  "##.##" "#",
                  "###.." "#",
                  "###.#" "#",
                  "####." "#"})

(def input "#...##.#...#..#.#####.##.#..###.#.#.###....#...#...####.#....##..##..#..#..#..#.#..##.####.#.#.###")
(def rules {"....." ".",
               "..#.." "#",
               "..##." "#",
               "#..##" ".",
               "..#.#" "#",
               "####." ".",
               "##.##" ".",
               "#...." ".",
               "###.." "#",
               "#####" "#",
               "##..#" "#",
               "#.###" "#",
               "#..#." "#",
               ".####" "#",
               "#.#.." "#",
               ".###." "#",
               ".##.." "#",
               ".#..." "#",
               ".#.##" "#",
               "##..." "#",
               "..###" ".",
               "##.#." ".",
               "...##" ".",
               "....#" ".",
               "###.#" ".",
               "#.##." "#",
               ".##.#" ".",
               ".#..#" "#",
               "#.#.#" "#",
               ".#.#." "#",
               "...#." "#",
               "#...#" "#"})

(def initial-padding 12)

(defn expand
  [input padding]
  (let [padded-str (cs/join (repeat padding "."))]
    (str padded-str input padded-str)))

(defn get-all-segments
  [input-str]
  (loop [input input-str
         output []]
    (let [next-segment (take 5 input)]
      (if (< (count next-segment) 5)
        output
        (recur (rest input) (conj output next-segment))))))

(defn handle-segment
  [rules segment]
  (let [string-segment (cs/join segment)
        mapping (get rules string-segment)]
    (if (nil? mapping) "." mapping)))

(defn simulate-generation
  [{:keys [initial-state rules]}]
  (let [segments (-> initial-state
                     (expand 4)
                     get-all-segments)]
    {:initial-state (cs/join (map #(handle-segment rules %) segments)) :rules rules}))

(defn pot-value
  [pot-num contents]
  (if (= "#" (str contents))
    pot-num
    0))

(defn sum-generation
  [generations gen-number]
  (let [target-generation (cs/join (:initial-state (last (take (inc gen-number) generations))))
        min (- 0 (* gen-number 2))
        max (+ (count target-generation) min)
        indices (range min max)]
    (->> target-generation
         (map pot-value indices)
         (reduce +))))

(defn process-generations
  [input rules]
   (iterate simulate-generation {:initial-state input :rules rules}))

(comment
  (get-all-segments (expand small-input))
  (simulate-generation {:initial-state (expand small-input) :rules small-rules}))