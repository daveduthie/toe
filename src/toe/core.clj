(ns toe.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; # Create board
(defn new-board [size]
  (vec (repeat size (vec (repeat size :-)))))

(defn get-char [i]
  (char (+ 65 i)))

(defn get-int [letter]
  (let [c (if (string? letter)
            (first (str/upper-case letter))
            letter)]
    (- (int c) 65)))

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
           nil))))

(defn get-input [{:keys [msg type default-val]
                  :or   {msg nil type :default default-val nil}}]
  (if msg (println msg))
  (let [input (clojure.string/trim (read-line))]
    (cond
      (not-empty input)
      (case type
        :int     (if-let [i (parse input)]
                   i
                   (get-input {:msg         "Doesn't look like an integer"
                               :type        type
                               :default-val default-val}))
        :char    (if-let [c (first (str/upper-case input))]
                   c
                   (get-input {:msg         "Doesn't look like a letter"
                               :type        type
                               :default-val default-val}))
        :mixed   (let [i (parse (re-find #"\d" input))
                       c (first (re-find #"[A-z]" (str/upper-case input)))]
                   (if-not (or (nil? i) (nil? c))
                     [(dec i) (get-int c)]
                     (get-input {:msg  "Try entering a letter and a number"
                                 :type :mixed})))
        :default input)

      (not (nil? default-val))
      default-val

      :else
      (get-input {:msg msg :type type :default-val default-val}))))

(defn replay? []
  (let [response (get-input {:msg "Play again? (YES/no)" :type :char :default-val \Y})]
    (cond (= \Y response) true
          (= \N response) false
          :else           (replay?))))

;; # Update board
(defn update-board [board player & message]
  (if (seq? message) (apply println message))
  (let [max-index (dec (count board))
        [row col] (get-input {:msg "\nWhere would you like to move?" :type :mixed})]

    (cond
      (and (every? #(<= 0 % max-index) [row col])
           (= :- (get-in board [row col])))
      (assoc-in board [row col] player)

      :else
      (update-board board player "That move appears to be impossible"))))

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
      (= :draw r)       (do (println "It's a DRAW") nil)
      :else             (do (println "The winner is" (re-find #"\w" (str r))) r))))

(str :a)

(defn new-game []
  (loop [score {:x 0 :o 0}]
    (let [size       (get-input {:msg "What size grid would you like? DEFAULT: 3" :type :int :default-val 3})
          win-length (get-input {:msg (str "How many symbols in a row to win? DEFAULT:" size) :type :int :default-val size})
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
