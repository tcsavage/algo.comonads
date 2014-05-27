(ns algo.comonads.universe
  (:require [algo.comonads :refer :all]))

(defn u-left [{focus :focus left :left right :right}] {:focus (first left) :left (rest left) :right (cons focus right)})
(defn u-right [{focus :focus left :left right :right}] {:focus (first right) :right (rest right) :left (cons focus left)})
(defn u-show [n u] (concat (reverse (take n (:left u)))
                           [\| (:focus u) \|]
                           (take n (:right u))))

(def w-universe-duplicate (fn [u] {:focus u :left (rest (iterate u-left u)) :right (rest (iterate u-right u))}))
(def w-universe-fmap (fn [f {focus :focus left :left right :right}] {:focus (f focus) :left (map f left) :right (map f right)}))

(defcomonad universe-w
  [w-extract (fn [{focus :focus}] focus)
   w-duplicate w-universe-duplicate
   w-fmap w-universe-fmap
   w-extendx (fn [f w] (w-universe-fmap f (w-universe-duplicate w)))])

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

(def u0 {:focus 1 :left [0 0 0 0 0] :right [0 0 0 0 0]})

(with-comonad universe-w
  (u-show 5
          (last
           (take 4
                 (iterate (partial w-extend (make-ca rule-110)) u0)))))
