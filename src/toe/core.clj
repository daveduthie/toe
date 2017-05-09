(ns toe.core
  (:gen-class)
  (:require [clojure.string :as str]
            [toe.utils :refer :all]
            [toe.ai :as ai]))

;; # Create board
(defn new-board [size]
  (vec (repeat size (vec (repeat size :-)))))

;; # Update board
(defn update-board-human [player state & message]
  (if (seq? message) (apply println message))
  (let [board     (:board state)
        pos       (read-move (format "Where would you like to move, %s?" (name player)))]
    (if (legal? board pos)
      (assoc state :board (assoc-in board pos player))
      (update-board-human player state "That move appears to be impossible"))))

(defn update-board-computer [player opponent {:keys [board win-len search-depth] :as state}]
  (assoc state :board (ai/best-move-for [player opponent] board win-len search-depth)))

(defn update-board [{:keys [players computer players] :as state}]
  (let [p1 (first players)
        p2 (second players)
        state' (assoc state :players (rest players))]
    (if (= p1 computer)
      (update-board-computer p1 p2 state')
      (update-board-human p1 state'))))

;; # Run game
(defn game [{:keys [board players win-len] :as state}]
  (let [result (result board win-len)]
    (render-board board)
    (condp = result
      :unfinished (game (update-board state))
      :draw       (do (println "It's a DRAW") nil)
      (do (println "The winner is" (name result)) result))))

(defn new-game []
  (loop [score {:x 0 :o 0}]
    (clear-screen)
    (let [size          (or (read-int "What size grid would you like? DEFAULT: 3") 3)
          win-len       (or (read-int (str "How many symbols in a row to win? DEFAULT:" size))
                            size)
          players       (cycle [:x :o])
          initial-state {:win-len      win-len
                         :board        (new-board size)
                         :players      players
                         :computer     (choose-computer-player (take 2 players))
                         :search-depth 4}
          winner        (game initial-state)
          score'        (if-not (nil? winner) (update score winner inc) score)]
      (println "Score: x" (:x score') " o" (:o score'))
      (if (replay?)
        (recur score')
        (System/exit 0)))))

(defn -main
  "Entry Point"
  [& args]
  (new-game))
