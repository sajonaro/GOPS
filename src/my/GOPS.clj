(ns my.GOPS
  (:gen-class))

(defn select-rand [col]
  (nth col (rand (count col))))

(defn without [col value]
  (filterv #(not= % value) col))

(defn play-random-strategy [cards]
  (select-rand cards))

(defn win-message [score1 score2]
  (cond
    (> score1 score2) "player1 won!"
    (< score1 score2) "player2 won!"
    :else "it's a tie!"))

(defn end-message [scores]
  (let [[score1 score2] scores]
    (str "\nScores:\n"
         (format "player1 - %s,  player2 - %s" score1 score2)
         "\n"
         (win-message score1 score2)
         "\n")))

(defn new-scores [oldScores playedCards]
  (let [[s1 s2] oldScores [c1 c2] playedCards]
    (cond
      (> c1 c2) [(inc s1) s2]
      (< c1 c2) [s1 (inc s2)]
      :else [s1 s2])))

(defrecord State
           [turnNumber
            bountyDeck
            p1Deck
            p2Deck
            scores
            bountyCard
            p1LastPlayed
            p2LastPlayed])

(defn turn-message [state]
  (str (format "Turn %s:" (:turnNumber state))
       "\n"
       (format "Player 1 played - %s" (:p1LastPlayed state))
       "\n"
       (format "Player 2 played - %s" (:p2LastPlayed state))))

;;; " state is record State   "
(defn next-state [state]
  (let [bountyCard (select-rand (:bountyDeck state))
        p1 (play-random-strategy (:bountyDeck state))]
    (->State (inc (:turnNumber state))
             (without (:bountyDeck state) bountyCard)
             ;;;p1 plays randomly
             (without (:p1Deck state) p1)
             ;;;p2 plays "equal" strategy
             (without (:p2Deck state) bountyCard)
             (new-scores (:scores state)
                         [p1 bountyCard])
             bountyCard
             p1
             bountyCard)))

(defn run-game []
  (loop [states [(->State 0
                          [1 2 3 4 5 6 7 8]
                          [1 2 3 4 5 6 7 8]
                          [1 2 3 4 5 6 7 8]
                          [0 0]
                          nil
                          nil
                          nil)]]
    (if (=  (count (:bountyDeck (last states))) 0)
      (do 
        (println (turn-message (last states)))
        (println (end-message (:scores (last states)))))
      (do
        (when (not= (:turnNumber (last states)) 0)
          (println (turn-message (last states))))
        (recur (conj states (next-state (last states))))))))


(defn -main
  [& args]
  (run-game))