(ns
  #^{:author "Tom Savage"}
  examples.cellular-automata
  (:require [algo.comonads :refer :all]
            [algo.comonads.universe :refer :all]))

(def rule-110 {[1 1 1] 0
               [1 1 0] 1
               [1 0 1] 1
               [1 0 0] 0
               [0 1 1] 1
               [0 1 0] 1
               [0 0 1] 1
               [0 0 0] 0})

(defn make-ca
  [rule]
  (fn [{focus :focus
        [l0 & _] :left
        [r0 & _] :right}]
    (or
     (get rule [l0 focus r0])
     0)))

(def u0 {:focus 1
         :left (take 10 (repeat 0))
         :right (take 10 (repeat 0))})

(defn run-cellular-automaton
  (with-comonad universe-w
    (doseq [i (take 10 (iterate (partial w-extend (make-ca rule-110)) u0))]
      (println (u-show 9 i)))))
