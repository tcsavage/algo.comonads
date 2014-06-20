algo.comonads
=============

Clojure comonads. Based on the `algo.monads` package.

[![Clojars Project](http://clojars.org/tcsavage/algo.comonads/latest-version.svg)](http://clojars.org/tcsavage/algo.comonads)

Usage
-----

Every concept in category theory has an opposite or "dual" formed by refersing the direction of morphisms. These duals are denoted with the "co-" prefix. Just like monads generalise the concept of a computational context, comonads generalise the concept of a machine.

Comonads must define the following functions:

* `w-extract :: Comonad w => w a -> a` takes a comonadic value and returns the value "in focus"
* `w-duplicate :: Comonad w => w a -> w (w a)` wraps the comonadic value in another layer
* `w-fmap :: Comonad w => (a -> b) -> w a -> w b` comonads are also functors. This works just like any other `fmap`

The functions `w-extract` and `w-duplicate` are duals of `m-result` and `m-join` respectively. The comonadic function `w-extend` is the dual of `m-bind`. Whereas monads are usually defined in terms of `m-result` and `m-bind` (letting `m-join` be defined as `(partial m-bind identity)`), I have made the decision to define comonads in terms of `w-extend` and `w-duplicate` instead of `w-extend` (letting `w-extend` be defined as `(fn [f w] (fmap f (w-duplicate w)))`) because it makes the implementation a little easier to work out.

See the [universe](https://github.com/tcsavage/algo.comonads/blob/master/src/algo/comonads/universe.clj) comonad implementation and the [cellular automata](https://github.com/tcsavage/algo.comonads/blob/master/src/examples/cellular_automata.clj) example for a fun use case.
