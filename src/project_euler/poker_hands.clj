(ns project-euler.poker-hands
  (:require [project-euler.poker-hands-input :as input]
            [clojure.string :as cs]
            [clojure.pprint :as pp]))

(def card-to-value {"2" 2
                    "3" 3
                    "4" 4
                    "5" 5
                    "6" 6
                    "7" 7
                    "8" 8
                    "9" 9
                    "T" 10
                    "J" 11
                    "Q" 12
                    "K" 13
                    "A" 14})

(def input-pattern #"(([1-9TKQJA][CSHD][\s]?){5})(([1-9TKQJA][CSHD][\s]?){5})")

(def card-pattern #"([1-9TKQJA][CSHD]).([1-9TKQJA][CSHD]).([1-9TKQJA][CSHD]).([1-9TKQJA][CSHD]).([1-9TKQJA][CSHD])")

(defn parse-cards [cards]
  (let [matcher (re-matcher card-pattern cards)
        cards (rest (re-find matcher))]
    (map #(assoc {} :card (str (first %)) :suit (str (last %))) cards)))

(defn parse-line [line]
  (let [matcher (re-matcher input-pattern line)
        matches (re-find matcher)
        player-one (parse-cards (cs/trim (nth matches 1)))
        player-two (parse-cards (cs/trim (nth matches 3)))]
    (assoc {} :playerOne player-one :playerTwo player-two)))

(defn parse-input [line-vector]
  (map parse-line line-vector))

(defn assign-card-value [card-map]
  (let [card (:card card-map)
        card-value (get card-to-value card)]
    (assoc card-map :card-value card-value)))

(defn assign-card-values [cards]
  (let [playerOne (:playerOne cards)
        playerTwo (:playerTwo cards)]
    (assoc {} :playerOne (map assign-card-value playerOne)
              :playerTwo (map assign-card-value playerTwo))))

(defn count-map [partition]
  (assoc (first partition) :count (count partition)))

(defn count-cards [cards]
  (let [sorted-cards (sort-by :card cards)
        partitioned-cards (partition-by :card sorted-cards)]
    (sort-by :count (map count-map partitioned-cards ))))

(defn is-straight [cards]
  (let [sorted-cards (sort (map :card-value cards))
        expected-straight (take 5 (range (first sorted-cards) 20))]
    (= sorted-cards expected-straight)))

(defn is-flush [cards]
  (let [suits (map :suit cards)]
    (every? #(= (first suits) %) suits)))

(defn is-four-of-a-kind [cards]
  (let [card-counts (count-cards cards)
        largest-set-size (get :count (last card-counts))]
    (= largest-set-size 4)))

(defn is-full-house [cards]
  (let [card-counts (count-cards cards)
        largest-set-size (get :count (last card-counts))
        smallest-set-size (get :count (first card-counts))]
    (and (= smallest-set-size 2) (= largest-set-size 3))))

(defn is-three-of-a-kind [cards]
  (let [card-counts (count-cards cards)
        largest-set-size (get :count (last card-counts))]
    (= largest-set-size 3)))

(defn is-two-pair [cards]
  (let [card-counts (count-cards cards)
        largest-set-size (get :count (last card-counts))
        smallest-set-size (get :count (first card-counts))
        second-set-size (get :count (nth card-counts 1))]
    (and (= smallest-set-size 1) (= second-set-size 2) (= largest-set-size 2))))

(defn is-one-pair [cards]
  (let [card-counts (count-cards cards)
        largest-set-size (get :count (last card-counts))]
    (= largest-set-size 2)))

(defn get-tiebreaker [cards]
  (let [card-counts (count-cards cards)
        hand-type (get-in cards [:hand :type])]
    (case hand-type
      (:four-of-a-kind :full-house :three-of-a-kind :one-pair) (get :card-value (last (sort-by :count card-counts)))
      :two-pair (get :card-value (last (sort-by :card-value (filter #(= 2 (get :count %))))))
      (get :card-value (last (sort-by :card-value card-counts))))))

(defn identify-hand [cards]
  (cond (and (is-straight cards) (is-flush cards)) {:type :straight-flush :rank 1 :tiebreaker}
        (is-four-of-a-kind cards) {:type :four-of-a-kind :rank 2}
        (is-full-house cards) {:type :full-house :rank 3}
        (is-flush cards) {:type :flush :rank 4}
        (is-straight cards) {:type :straight :rank 5}
        (is-three-of-a-kind cards) {:type :three-of-a-kind :rank 6}
        (is-two-pair cards) {:type :two-pair :rank 7}
        (is-one-pair cards) {:type :one-pair :rank 8}
        :else {:type :high-card :rank 9}))

(defn identify-hands [input]
  (let [playerOneCards (:playerOne input)
        playerTwoCards (:playerTwo input)]
    (assoc {} :playerOne {:cards playerOneCards
                          :hand  (identify-hand playerOneCards)}
              :playerTwo {:cards playerTwoCards
                          :hand  (identify-hand playerTwoCards)})))

(defn determine-winner [input]
  (let [playerOneHand (get-in input [:playerOne :hand])
        playerTwoHand (get-in input [:playerTwo :hand])
        playerOneRank (get playerOneHand :rank)
        playerTwoRank (get playerTwoHand :rank)]
    (cond (> playerOneRank playerTwoRank) :playerTwo
          (< playerOneRank playerTwoRank) :playerOne
          :else :tie)))

(comment
  (map assign-card-values full-input))
