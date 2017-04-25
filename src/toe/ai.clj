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

(defn reverse-vals
  "Flip `:val` in a map"
  [m]
  (update m :val (partial * -1)))

;; (defn max-val [depth evaluations]
;;   (if (= depth 3)
;;     (apply max-key :val evaluations)
;;     (apply max (map :val evaluations))))

(defn max-val [depth evaluations]
  (apply max (map :val evaluations)))

;; Negamax correctly returns the value.
;; Now I need to to return the associated move!
(defn negamax [[player opponent] board win-len depth]
  ;; memoize here?
  (let [r (result board win-len)]
    (if (or (not= :unfinished r) (zero? depth))
      {:board board :val (static-value player r)}
      {:board board
       :val   (max-val
               depth
               (map (comp reverse-vals
                          (fn [pos] (negamax [opponent player] (assoc-in board pos player) win-len (dec depth))))
                    (legal-moves board)))})))

;; # SCRATCHPAD

;; (let [col [{:b 'any :val 0}
;;            {:b 'old :val -1}
;;            {:b 'thing :val 1}]]
;;   (apply max-key :val col))

;; (let [col [{:b 'any :val 0}
;;            {:b 'old :val -1}
;;            {:b 'thing :val 1}]]
;;   (map (fn [m] (update m :val (partial * -1)))
;;        col))

(let [b [[:x :o :-]
         [:- :x :-]
         [:- :- :o]]]
  (negamax [:x :o] b 3 4))
