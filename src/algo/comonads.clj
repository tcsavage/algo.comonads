(ns algo.comonads
  (:use [clojure.tools.macro :only (with-symbol-macros defsymbolmacro name-with-attributes)]))

(defmacro comonad
  "Define a comonad by defining the comonad operations. The definitions
   are written like bindings to the comonad operations w-extend and
   w-extract (required)"
  [operations]
  `(let [~'w-extract   ::this-comonad-does-not-define-w-extract
         ~'w-duplicate ::this-comonad-does-not-define-w-extract
         ~'w-fmap      ::this-comonad-does-not-define-w-fmap
         ~'w-extend    ::this-comonad-does-not-define-w-extend
         ~@operations]
     {:w-extract ~'w-extract
      :w-duplicate ~'w-duplicate
      :w-fmap ~'w-fmap
      :w-extend ~'w-extend}))

(defsymbolmacro w-extract w-extract)
(defsymbolmacro w-duplicate w-duplicate)
(defsymbolmacro w-fmap w-fmap)
(defsymbolmacro w-extend w-extend)

(defmacro defcomonad
  "Define a named comonad by defining the comonad operations. The definitions
   are written like bindings to the comonad operations w-extend and
   w-extract (required)."

  ([name doc-string operations]
   (let [doc-name (with-meta name {:doc doc-string})]
     `(defcomonad ~doc-name ~operations)))

  ([name operations]
   `(def ~name (comonad ~operations))))

(defmacro defcomonadfn
  "Like defn, but for functions that use comonad operations and are used inside
   a with-comonad block."
  {:arglists '([name docstring? attr-map? args expr]
               [name docstring? attr-map? (args expr) ...])}
  [name & options]
  (let [[name options]  (name-with-attributes name options)
        fn-name (symbol (str *ns*) (format "w+%s+w" (str name)))
        make-fn-body    (fn [args expr]
                          (list (vec (concat ['w-extract 'w-duplicate
                                              'w-fmap 'w-extend] args))
                                (list `with-symbol-macros expr)))]
    (if (list? (first options))
      ; multiple arities
      (let [arglists        (map first options)
            exprs           (map second options)
            ]
        `(do
           (defsymbolmacro ~name (partial ~fn-name ~'w-extract ~'w-duplicate
                                                   ~'w-fmap ~'w-extend))
           (defn ~fn-name ~@(map make-fn-body arglists exprs))))
      ; single arity
      (let [[args expr] options]
        `(do
           (defsymbolmacro ~name (partial ~fn-name ~'w-extract ~'w-duplicate
                                                   ~'w-fmap ~'w-extend))
           (defn ~fn-name ~@(make-fn-body args expr)))))))

(defmacro with-comonad
  "Evaluates an expression after replacing the keywords defining the
   comonad operations by the functions associated with these keywords
   in the comonad definition given by name."
  [comonad & exprs]
  `(let [name#         ~comonad
         ~'w-extract   (:w-extract name#)
         ~'w-duplicate (:w-duplicate name#)
         ~'w-fmap      (:w-fmap name#)
         ~'w-extend    (:w-extend name#)]
     (with-symbol-macros ~@exprs)))

(def w-store-fmap (fn [f {stored :stored accessor :accessor}] {:stored stored :accessor #(f (accessor %))}))
(def w-store-duplicate (fn [{stored :stored accessor :accessor}] {:stored stored :accessor (fn [x] {:stored x :accessor accessor})}))

(defcomonad store-w
  "Comonad constructed of a value `s` and a function `s -> a`."
  [w-extract (fn [{stored :stored accessor :accessor}] (accessor stored))
   w-duplicate w-store-duplicate
   w-fmap w-store-fmap
   w-extend (fn [f w] (w-store-fmap f (w-store-duplicate w)))])

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
   w-extend (fn [f w] (w-universe-fmap f (w-universe-duplicate w)))])

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
