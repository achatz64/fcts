(ns fcts.core
  (:refer-clojure :exclude [and]))

(defmacro my [a]
  `(cljs.core/println ~a))

(defmacro lift-cljs []
  (let [to-read (loop [l '(c/first)
                       m '(first)
                       r '()]
                  (if l
                    (recur (next l) (next m) (conj r (list 'def (first m) (list 'fcts.core/lift* (first l)))))
                    r))]
    `(do ~@to-read)))
