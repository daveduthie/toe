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

(def test-board-3
  [[:a :b :c :d]
   [:e :f :g :h]
   [:i :j :k :l]
   [:m :n :o :p]])

(def test-board-4 ;; it's a draw
  [[:x :- :o]
   [:o :x :x]
   [:x :o :o]])


(deftest diagonals-test

  (testing "right-&-left"
    (is (= (right-&-left-diagonals test-board-3 0 2)
           [[:c :h] [:c :f :i]])))

  (testing "all diagonals"
    (is (= (diagonals test-board-3 3)
           [[:a :f :k :p] [:b :g :l] [:c :f :i] [:d :g :j :m] [:e :j :o] [:h :k :n]]))))

;; returns true if a player has at least `win-length` in a row
(deftest winner-test
  (is (= :o
         (winner test-board-1 3)))
  (is (= :x
         (winner test-board-2 3))))

;; returns true if no squares are unfilled
(deftest draw-test
  (is (false?
       (draw? test-board-2 3))))

(deftest winnable-test
  (let [diag (first (diagonals test-board-4 3))]
    (is (false? (winnable? diag 3)))))
