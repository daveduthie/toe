(ns toe.ai-test
  (:require [clojure.test :refer :all]
            [toe.ai :refer :all]))

(def test-board-1
  [[:x :o :o]
   [:- :x :-]
   [:x :- :o]])

(deftest best-move-test
  (is (= ((negamax [:x :o] test-board-1 3 3) :board)
         [[:x :o :o]
          [:x :x :-]
          [:x :- :o]])))
