(ns toe.ai-test
  (:require [clojure.test :refer :all]
            [toe.ai :refer :all]))

(def test-board-1
  [[:x :o :o]
   [:- :x :-]
   [:x :- :o]])

(def test-board-2
  [[:- :- :-]
   [:- :- :-]
   [:- :- :x]])

(deftest best-move-test
  (is (= (best-move-for [:x :o] test-board-1 3 3)
         [[:x :o :o]
          [:x :x :-]
          [:x :- :o]]))
  (is (= (best-move-for [:o :x] test-board-2 3 7)
         [[:- :- :-]
          [:- :o :-]
          [:- :- :x]])))
