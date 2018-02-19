(ns fcts.core
  (:refer-clojure :exclude [and]))

(defmacro my [a]
  `(cljs.core/println ~a))
