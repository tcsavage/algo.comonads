(ns
  #^{:author "Tom Savage"}
  algo.comonads.universe
  (:require [algo.comonads :refer :all]))

(defn u-left [{focus :focus left :left right :right}] {:focus (first left) :left (rest left) :right (cons focus right)})
(defn u-right [{focus :focus left :left right :right}] {:focus (first right) :right (rest right) :left (cons focus left)})
(defn u-show [n u] (concat (reverse (take n (:left u)))
                           [\| (:focus u) \|]
                           (take n (:right u))))

(defcomonad universe-w
  [w-extract (fn [{focus :focus}] focus)
   w-duplicate (fn [u] {:focus u
                        :left (rest (iterate u-left u))
                        :right (rest (iterate u-right u))})
   w-fmap (fn [f {focus :focus left :left right :right}] {:focus (f focus)
                                                          :left (map f left)
                                                          :right (map f right)})])
