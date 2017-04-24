(ns toe.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; # Create board
(defn new-board [size]
  (vec (repeat size (vec (repeat size :-)))))

;; # Draw board on screen
(defn int->char [i]
  (char (+ 65 i)))

(defn clear-screen []
  (println (str (char 27) "[2J"))  ; clear screen
  (println (str (char 27) "[;H"))) ; set cursor to top

(defn render-board [b & msg]
  (clear-screen)
  (let [max-index (count b)]
    (if msg (apply println msg))
    (println (str/join (map #(format "   %c" (int->char %)) (range max-index)))) ; print letters along top
    (doseq [row (range max-index)]
      (print (format "%02d" (inc row))) ; print numbers along left
      (doseq [col (range max-index)]
        (print (-> (str (get-in b [row col]))
                   (clojure.string/replace #"[:-]" " ")))
        (if (< col (dec max-index)) (print " |")))
      (if (< row (dec max-index))
        (println "\n " (apply str (repeat (dec (* 4 max-index)) "-")))
        (println)))))

;; # Get input
(defn parse [x]
  (if (integer? x)
    x
    (try (Integer/parseInt x)
         (catch NumberFormatException e
           nil))))

(defn char->int [letter]
  (let [c (if (string? letter)
            (first (str/upper-case letter))
            letter)]
    (- (int c) 65)))

(defn read-int
  "May return nil. Call as `(or (read-int msg) default-value)`"
  [msg]
  (if msg (println msg))
  (some-> (read-line) str/trim parse))

(defn read-char [msg]
  (loop [msg msg]
    (if msg (println msg))
    (if-let [c (-> (read-line) str/trim str/upper-case first)]
      c
      (recur "We're looking for letters"))))

(defn read-move [msg]
  (loop [msg msg]
    (if msg (println msg))
    (let [input (read-line)
          i (parse (re-find #"\d" input))
          c (first (re-find #"[A-z]" (str/upper-case input)))]
      (if (not-any? nil? [i c])
        [(dec i) (char->int c)]
        (recur "We'd like to accomodate your choice but how can we?")))))

(defn choose-computer-player []
  (let [choice (read-char "Should the computer play one part? (x o no)")]
    (condp = choice
      \X :x
      \O :o
      nil)))

(defn replay? []
  (let [response (read-char "Play again? (YES/no)")]
    (condp = response
      \Y           true
      \N           false
      (replay?))))

;; # Update board
(defn legal? [board [row col]]
  (let [max-index (dec (count board))]
    (and (every? #(<= 0 % max-index) [row col])
         (= :- (get-in board [row col])))))

(defn update-board-human [board player message]
  (if (seq? message) (apply println message))
  (let [max-index (dec (count board))
        pos (read-move (format "Where would you like to move, %s?" (str (name player))))]

    (cond
      (legal? board pos)
      (assoc-in board pos player)

      :else
      (update-board-human board player "That move appears to be impossible"))))

(defn update-board-computer [board player]
  (loop [max-tries 0]
    (if (> max-tries 10000)
      (do (println "I'm tired...") board)
      (let [max-index (count board)
            pos       [(rand-int max-index) (rand-int max-index)]]
        (if (legal? board pos)
          (assoc-in board pos player)
          (recur (inc max-tries)))))))

(defn update-board [board player & {:keys [message computer]}]
  (if (= player computer)
      (update-board-computer board player)
      (update-board-human board player message)))

;; # Test game-over conditions

;; ## build representation of all "slices" of board
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

(defn right-&-left-diagonals [b x-coord y-coord]
  (let [size (count b)
        diag (fn [f]
               (loop [elems []
                      x x-coord
                      y y-coord]
                 (if-let [e (get-in b [x y])]
                   (recur (conj elems e)
                          (inc x)
                          (f y))
                   elems)))]
    (map diag [inc dec])))

(defn diagonals [b win-length]
  (let [size (count b)]
    (filter #(>= (count %) win-length)
            (concat
             ;; top row
             (mapcat (fn [y] (right-&-left-diagonals b 0 y))
                     (range size))
             ;; walk down edges
             (mapcat (fn [x y] (right-&-left-diagonals b x y))
                     (mapcat (partial repeat 2) (range 1 size))
                     (cycle [0 (dec size)]))))))

;; ## Really test win-lose-draw-unfinished conditions
(defn winner
  "Returns player with 'win-length` in a row, or nil if neither players has won"
  [b win-length]
  (let [size       (count b)
        slices (concat (rows b)
                       (columns b)
                       (diagonals b win-length))]
    (some (fn [slice]
            (some #(and (>= (count %) win-length)
                        (not= :- (first %))
                        (first %))
                  (partition-by identity slice)))
          slices)))

(defn winnable?
  "Tests if a sequence can accomodate `length` of the same piece in a row"
  [slice length]
  (loop [[head & tail] slice
         x             0
         o             0]
    (cond (or (= length x) (= length o)) true
          (nil? head)                    false

          (= head :x) (recur tail (inc x) 0)
          (= head :o) (recur tail 0 (inc o))
          :else       (recur tail (inc x) (inc o)))))

(defn draw?
  "Returns true if neither player can possibly get `win-length` in a row."
  [board win-length]
  (let [slices (concat (rows board)
                       (columns board)
                       (diagonals board win-length))]
    (not-any? #(winnable? % win-length)
              slices)))

(defn result [board win-length]
  (if-let [w (winner board win-length)]
    w
    (if (draw? board win-length) :draw :unfinished)))

;; # Run game
(defn game [board win-length players computer]
  (render-board board)
  (let [r (result board win-length)]
    (condp = r
      :unfinished (game (update-board board (first players) :computer computer)
                        win-length
                        (next players)
                        computer)
      :draw       (do (println "It's a DRAW") nil)
      (do (println "The winner is" (name r)) r))))

(defn new-game []
  (loop [score {:x 0 :o 0}]
    (clear-screen)
    (let [size        (or (read-int "What size grid would you like? DEFAULT: 3") 3)
          win-length  (or (read-int (str "How many symbols in a row to win? DEFAULT:" size)) size)
          computer    (choose-computer-player)
          winner      (game (new-board size) win-length (cycle [:x :o]) computer)
          score'      (if-not (nil? winner) (update score winner inc) score)]
      (println "Score: x" (:x score') " o" (:o score'))
      (if (replay?)
        (recur score')
        (System/exit 0)))))

(defn -main
  "Entry Point"
  [& args]
  (new-game))
