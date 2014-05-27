(ns algo.comonads
  (:use [clojure.tools.macro :only (with-symbol-macros defsymbolmacro name-with-attributes)]))

(defmacro comonad
  "Define a comonad by defining the comonad operations. The definitions
   are written like bindings to the comonad operations w-extract, w-duplicate and
   w-fmap (required)."
  [operations]
  `(let [~'w-extract   ::this-comonad-does-not-define-w-extract
         ~'w-duplicate ::this-comonad-does-not-define-w-extract
         ~'w-fmap      ::this-comonad-does-not-define-w-fmap
         ~@operations]
     {:w-extract ~'w-extract
      :w-duplicate ~'w-duplicate
      :w-fmap ~'w-fmap}))

(defsymbolmacro w-extract w-extract)
(defsymbolmacro w-duplicate w-duplicate)
(defsymbolmacro w-fmap w-fmap)

(defmacro defcomonad
  "Define a named comonad by defining the comonad operations. The definitions
   are written like bindings to the comonad operations w-extract, w-duplicate and
   w-fmap (required)."

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
                          (list (vec (concat ['w-extract 'w-duplicate 'w-fmap] args))
                                (list `with-symbol-macros expr)))]
    (if (list? (first options))
      ; multiple arities
      (let [arglists        (map first options)
            exprs           (map second options)
            ]
        `(do
           (defsymbolmacro ~name (partial ~fn-name ~'w-extract ~'w-duplicate ~'w-fmap))
           (defn ~fn-name ~@(map make-fn-body arglists exprs))))
      ; single arity
      (let [[args expr] options]
        `(do
           (defsymbolmacro ~name (partial ~fn-name ~'w-extract ~'w-duplicate ~'w-fmap))
           (defn ~fn-name ~@(make-fn-body args expr)))))))

(defmacro with-comonad
  "Evaluates an expression after replacing the keywords defining the
   comonad operations by the functions associated with these keywords
   in the comonad definition given by name."
  [comonad & exprs]
  `(let [name#         ~comonad
         ~'w-extract   (:w-extract name#)
         ~'w-duplicate (:w-duplicate name#)
         ~'w-fmap      (:w-fmap name#)]
     (with-symbol-macros ~@exprs)))

(defcomonadfn w-extend
  "Generate a new value from the context."
  [f w]
  (w-fmap f (w-duplicate w)))
