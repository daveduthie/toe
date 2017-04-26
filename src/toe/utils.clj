(ns toe.utils
  (:require [clojure.string :as str]))

(defn legal? [board [row col]]
  (let [max-index (dec (count board))]
    (and (every? #(<= 0 % max-index) [row col])
         (= :- (get-in board [row col])))))

(defn int->char [i]
  (char (+ 65 i)))

;; # Draw board on screen
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

(defn choose-computer-player [players]
  (let [choice         (read-char "Should the computer play one part? (x o no)")
        compare-choice (fn [player] (if (= choice (first (str/upper-case (name player))))
                                      player
                                      nil))]
    (some compare-choice players)))

(defn replay? []
  (let [response (read-char "Play again? (yes/no)")]
    (condp = response
      \Y           true
      \N           false
      (replay?))))

;; ## build representation of all "slices" of board
(def rows
  (memoize
   (fn [b]
     (let [size (count b)]
       (for [x (range size)]
         (map #(get-in b %)
              (partition 2 (interleave (repeat x) (range size)))))))))

(def columns
  (memoize
   (fn [b]
     (let [size (count b)]
       (for [x (range size)]
         (map #(get-in b %)
              (partition 2 (interleave (range size) (repeat x)))))))))

(def right-&-left-diagonals
  (memoize
   (fn [b x-coord y-coord]
     (let [size (count b)
           diag (fn [f]
                  (loop [elems []
                         x     x-coord
                         y     y-coord]
                    (if-let [e (get-in b [x y])]
                      (recur (conj elems e)
                             (inc x)
                             (f y))
                      elems)))]
       (map diag [inc dec])))))

(def diagonals
  (memoize
   (fn [b win-length]
     (let [size (count b)]
       (filter #(>= (count %) win-length)
               (concat
                ;; top row
                (mapcat (fn [y] (right-&-left-diagonals b 0 y))
                        (range size))
                ;; walk down edges
                (mapcat (fn [x y] (right-&-left-diagonals b x y))
                        (mapcat (partial repeat 2) (range 1 size))
                        (cycle [0 (dec size)]))))))))

;; # Test game-over conditions

;; ## Test if there is a winner (and return the winner)
(def winner
  "Returns player with 'win-length` in a row, or nil if neither players has won"
  (memoize
   (fn [b win-length]
     (let [size   (count b)
           slices (concat (rows b)
                          (columns b)
                          (diagonals b win-length))]
       (some (fn [slice]
               (some #(and (>= (count %) win-length)
                           (not= :- (first %))
                           (first %))
                     (partition-by identity slice)))
             slices)))))

;; ## Test if game is drawn
(def winnable?
  "Tests if a sequence can accomodate `length` of the same piece in a row"
  (memoize
   (fn [slice length]
     (loop [[head & tail] slice
            x             0
            o             0]
       (cond (or (= length x) (= length o)) true
             (nil? head)                    false

             (= head :x) (recur tail (inc x) 0)
             (= head :o) (recur tail 0 (inc o))
             :else       (recur tail (inc x) (inc o)))))))

(def draw?
  "Returns true if neither player can possibly get `win-length` in a row."
  (memoize
   (fn [board win-length]
     (let [slices (concat (rows board)
                          (columns board)
                          (diagonals board win-length))]
       (not-any? #(winnable? % win-length)
                 slices)))))

;; ## Return outcome of game, or `:unfinished`
(def result
  (memoize
   (fn [board win-len]
     (if-let [w (winner board win-len)]
       w
       (if (draw? board win-len) :draw :unfinished)))))
