(ns toe.ai
  (:require [toe.utils :refer [legal? result]]))

(defn legal-moves [board]
  (let [rng   (range (count board))
        moves (for [row rng col rng] [row col])]
    (filter (partial legal? board) moves)))

(defn legal-moves-for
  [player board]
  (map (fn [pos] (assoc-in board pos player))
       (legal-moves board)))

(defn static-value [player result]
  (condp = result
    :draw       0
    :unfinished 0
    player      1
    -1))

;; Negamax correctly returns the value.
;; Now I need to to return the associated move!
(defn negamax [[player opponent] board win-len depth]
  ;; memoize here?
  ;; {:post [(not (nil? %))]}
  (let [r (result board win-len)]
    (if (or (not= :unfinished r) (zero? depth))
      (static-value player r)
      (apply max
             (map (fn [board'] (- (negamax [opponent player] board' win-len (dec depth))))
                  (legal-moves-for player board))))))

;; Now it seems to work but it's ugly.
;; negamax has 4 parameters
(defn best-move-for
  [[p1 p2] board win-len depth]
  (let [move (future
               (apply
                min-key
                (fn [board'] (negamax [p2 p1] board' win-len depth))
                (legal-moves-for p1 board)))]
    (Thread/sleep 700)
    (deref move)))
