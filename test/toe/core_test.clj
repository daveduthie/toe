(ns toe.core-test
  (:require [clojure.test :refer :all]
            [toe.core :refer :all]))

(def test-board-1
  [[:x :o :x]
   [:x :o :x]
   [:o :o :o]])

(def test-board-2
  [[:x :o :x]
   [:o :x :o]
   [:o :x :x]])

;; returns true if a player has at least `win-length` in a row
(deftest winner-test
  (is (= :o
         (winner test-board-1 3)))
  (is (= :x
         (winner test-board-2 3))))

;; returns true if no squares are unfilled
(deftest draw-test
  (is (true?
       (draw test-board-2))))
