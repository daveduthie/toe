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
  {:post [(integer? %)]}
  (condp = result
    :draw       0
    :unfinished 0
    player      1
    -1))

(defn flip [i]
  {:pre [(integer? i)]}
  (* -1 i))

(defn max-val [depth]
  (fn [d evaluations]
    {:pre [(integer? d)
           (seq? evaluations)
           (every? integer? evaluations)]
     :post [(not (nil? %))]}
    (println "evaluations: " evaluations)
    (apply max evaluations)))

;; Negamax correctly returns the value.
;; Now I need to to return the associated move!
(defn negamax [[player opponent] board win-len depth max-v]
  ;; memoize here?
  {:post [(not (nil? %))]}
  (let [r (result board win-len)]
    (if (or (not= :unfinished r) (zero? depth))
      (static-value player r)
      (max-v depth (map (comp flip
                              (fn [board']
                                (negamax [opponent player] board' win-len (dec depth) max-v)))
                        (legal-moves-for player board))))))

;; Now it seems to work but it's ugly.
;; negamax has 5 parameters!
;; (defn negamax-wrapper [[player opponent] board win-len depth]
;;   (let [get-max (max-val depth)]
;;     (:val (negamax [player opponent] board win-len depth get-max))))
(defn best-move-for
  [[p1 p2] board win-len depth]
  (let [max-v (max-val depth)]
    (apply min-key
           (fn [board'] (negamax [p2 p1] board' win-len depth max-v))
           (legal-moves-for p1 board))))
