;;;; TODO
;;   - [x] detect unwinnable games and report draw
;;   - [ ] draw row and column id's
;;   - [ ] extract number parser to separate fn to avoid duplication
(ns toe.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; # Create board
(defn new-board [size]
  (vec (repeat size (vec (repeat size :-)))))

(defn get-char [i]
  (char (+ 65 i)))

(defn get-int [upper]
  (- (int upper) 65))

;; # Draw board on screen
(defn render-board [b & msg]
  (let [max-index (count b)]
    (println (str (char 27) "[2J")) ; clear screen
    (println (str (char 27) "[;H")) ; set cursor to top
    (if msg (apply println msg))
    (apply println ; print letters along top
           (interleave (cycle " ")
                       (map #(get-char %) (range max-index))))
    (doseq [row (range max-index)]
      (print (inc row))  ; print numbers along left
      (doseq [col (range max-index)]
        (print (-> (str (get-in b [row col]))
                   (clojure.string/replace #"[:-]" " ")))
        (if (< col (dec max-index)) (print " |")))
      (if (< row (dec max-index))
        (println "\n" (apply str (repeat (dec (* 4 max-index)) "-")))
        (println)))))

;; # Get input
(defn parse [x]
  (if (integer? x)
    x
    (try (Integer/parseInt x)
         (catch NumberFormatException e
           (println (.getMessage e))))))

(defn get-input
  ([msg] (get-input msg nil))
  ([msg default-value]
   (println msg)
   (let [input (clojure.string/trim (read-line))]
     (cond
       (not-empty input)          (clojure.string/lower-case input)
       (not (nil? default-value)) default-value
       :else                      (get-input msg default-value)))))

;; #"[A-Z].*\d+"
(defn prompt-move
  ([] (prompt-move ""))
  ([message]
   (println message)
   (let [input  (get-input "\nWhere would you like to move? (column & row):")
         column (get-int (first (str/upper-case (re-find #"[A-z]" input))))
         row    (dec (parse (re-find #"\d+" input)))]
     [row column])))

(defn replay? []
  (let [response (get-input "Play again? (yes/no)")]
    (cond (= \y (first response)) true
          (= \n (first response)) false
          :else                    (replay?))))

;; # Update board
(defn update-board [b player]
  (let [max-index (dec (count b))
        [row col] (prompt-move)]
    (cond
      (some #(> % max-index) [row col])
      (do (render-board b (str "Enter numbers between " 0 " and " max-index))
          (update-board b player))

      (not= :- (get-in b [row col]))
      (do (render-board b "That square is not empty")
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
                     (interleave (range 1 size) (range 1 size))
                     (cycle [0 (dec size)]))))))

(defn winner [b win-length]
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

(defn winnable? [coll win-length]
  (loop [[head & tail] coll
         x             0
         o             0]
    (cond (or (= win-length x) (= win-length o)) true

          (nil? head) false

          (= head :x) (recur tail (inc x) 0)
          (= head :o) (recur tail 0       (inc o))
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
(defn game [board win-length players]
  (render-board board)
  (let [r (result board win-length)]
    (cond
      (= :unfinished r) (game (update-board board (first players))
                              win-length
                              (next players))
      (= :draw r)       nil
      :else             r)))

(defn new-game []
  (loop [score {:x 0 :o 0}]
    (let [size       (get-input "What size grid would you like? DEFAULT: 3" 3)
          win-length (get-input (str "How many symbols in a row to win? DEFAULT:" size) size)
          winner     (game (new-board (parse size)) (parse win-length) (cycle [:x :o]))]

      (let [score' (if-not (nil? winner)
                     (update score winner inc)
                     score)]
        (println "Score: x" (:x score') " o" (:o score'))
        (if (replay?)
          (recur score')
          (System/exit 0))))))

(defn -main
  "Entry Point"
  [& args]
  (new-game))
