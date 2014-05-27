(ns
  #^{:author "Tom Savage"}
  algo.comonads.store
  (:require [algo.comonads :refer :all]))

(def w-store-fmap (fn [f {stored :stored accessor :accessor}] {:stored stored :accessor #(f (accessor %))}))
(def w-store-duplicate (fn [{stored :stored accessor :accessor}] {:stored stored :accessor (fn [x] {:stored x :accessor accessor})}))

(defcomonad store-w
  "Comonad constructed of a value `s` and a function `s -> a`."
  [w-extract (fn [{stored :stored accessor :accessor}] (accessor stored))
   w-duplicate w-store-duplicate
   w-fmap w-store-fmap
   w-extendx (fn [f w] (w-store-fmap f (w-store-duplicate w)))])
