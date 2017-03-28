;; Requirements
;;
;;     - [x] get input
;;     - [x] validate input
;;     - [x] update board
;;     - [x] render board
;;     - [x] test win-draw conditions
(ns toe.core
  (:gen-class))

(declare new-game)

;; # Create board
(defn new-board [size]
  (vec (repeat size (vec (repeat size :-)))))

;; # Draw board on screen
(defn render-board [b]
  (let [max-index (count b)]
    (doseq [row (range max-index)]
      (print " ")
      (doseq [col (range max-index)]
        (print (-> (str (get-in b [row col]))
                   (clojure.string/replace #"[:-]" " "))
               "")
        (if (< col (dec max-index)) (print "|")))
      (if (< row (dec max-index))
        (println "\n" (apply str (repeat (dec (* 4 max-index)) "-")))
        (println)))))

;; # Get input
(defn get-input
  ([msg] (get-input msg nil))
  ([msg default-value]
   (println msg)
   (let [input (clojure.string/trim (read-line))]
     (cond
       (not-empty input)          (clojure.string/lower-case input)
       (not (nil? default-value)) default-value
       :else                      (get-input msg "")))))

(defn prompt-move
  ([] (prompt-move ""))
  ([message]
   (println message)
   (let [response (map #(Integer/parseInt %)
                       (re-seq #"\w+"
                               (get-input "\nWhere would you like to move? (row & column):")))]
     (if (= 2 (count response))
       response
       (prompt-move)))))

(defn prompt-replay []
  (let [response (get-input "Play again? (yes/no)")]
    (cond (= \y (first response)) (new-game)
          (= \n (first response)) (System/exit 0)
          :else                    (prompt-replay))))

;; # Update board
(defn update-board [b player]
  (let [max-index (dec (count b))
        [row col] (prompt-move)]
    (cond
      (some #(> % max-index) [row col])
      (do (println (str "Enter numbers between " 0 " and " max-index))
          (render-board b)
          (update-board b player))

      (not= :- (get-in b [row col]))
      (do (println "That square is not empty")
          (render-board b)
          (update-board b player))

      :else
      (assoc-in b [row col] player))))

;; # Test game-over conditions
(defn rows [b]
  (let [size (count b)]
    (for [x (range size)]
      (map #(get-in b %)
           (partition 2 (interleave (repeat x) (range size)))))))

(defn columns [b]
  (let [size (count b)]
    (for [x (range size)]
      (map #(get-in b %)
           (partition 2 (interleave (range size) (repeat x)))))))

(defn diagonal [b x-start y-start f]
  (let [size (count b)]
    (loop [elems []
           x x-start
           y y-start]
      (if (= x size)
        elems
        (recur (conj elems (get-in b [x y]))
               (inc x)
               (f y))))))

(defn diagonals [b]
  (let [size (count b)]
    (mapcat (fn [x] (vector (diagonal b x 0 inc)
                            (diagonal b x (dec size) dec)))
            (range size))))

(defn winner [b win-length]
  (let [size       (count b)
        win-slices (concat (rows b)
                           (columns b)
                           (take-while #(>= (count %) win-length)
                                       (diagonals b)))]
    (some (fn [slice]
            (some #(and (>= (count %) win-length)
                        (not= :- (first %))
                        (first %))
                  (partition-by identity slice)))
          win-slices)))

(defn draw [b]
  (not-any? #(= :- %)
            (for [x (range 3)
                  y (range 3)]
              (get-in b [x y]))))

(defn win-or-draw [b win-length]
  (if-let [w (winner b win-length)]
    (do (println "The winner is" (clojure.string/replace w #":" ""))
        true)
    (if (draw b)
      (do (println "It's a draw... ")
          true))))

;; # Run game
(defn game [board win-length players]
  (render-board board)
  (if (win-or-draw board win-length)
    (prompt-replay)
    (game (update-board board (first players))
          win-length
          (next players))))

(defn new-game []
  (letfn [(parse [x] (if (integer? x)
                       x
                       (try (Integer/parseInt x)
                            (catch NumberFormatException e
                              (new-game)))))]

    (let [size       (get-input "What size grid would you like? DEFAULT: 3" 3)
          win-length (get-input (str "How many symbols in a row to win? DEFAULT:" size) size)]

      (game (new-board (parse size)) (parse win-length) (cycle [:x :o])))))

(defn -main
  "Entry Point"
  [& args]
  (new-game))
