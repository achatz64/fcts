(ns fcts.core
  (:require-macros [fcts.core :refer [lift-cljs and or lazy-seq cond if-else fn let loop]])
  (:refer-clojure :exclude [->PersistentHashSet js->clj sort-by chunk-first print-meta? m3-hash-int pr-str* eduction tree-seq unchecked-remainder-int uuid seq reduce find-ns contains? every? ->ES6IteratorSeq keep-indexed ->PersistentQueueSeq subs set take-last bit-set qualified-keyword? ->Eduction ->ES6SetEntriesIterator -with-meta ->PersistentArrayMapIterator butlast unchecked-subtract-int -iterator take-nth first native-satisfies? seq? -sorted-seq-from println-str inst-ms iterate -empty newline -chunked-rest write-all fn? -prefer-method -assoc doall keyword-identical? prefers -js->clj dedupe ->ES6Iterator dissoc atom bit-shift-right -first peek aget -write iter mk-bound-fn last -default-dispatch-val pr namespace obj-map -conj = take vector? boolean bit-shift-left random-uuid any? rand-int aclone vreset! chunk dec ->TransformerIterator map juxt ->PersistentQueueIter < test rest ex-data -drop-first isa? boolean? -clone munge ->NeverEquiv re-seq char? make-hierarchy -reduce -count swap-vals! keep char mapcat unchecked-long some? unchecked-negate symbol-identical? reverse inst? range bit-count sort ->MetaFn unchecked-inc-int -compare map-indexed array-list rand-nth comp array-chunk dispatch-fn bit-shift-right-zero-fill -as-transient dorun pr-sequential-writer simple-symbol? disj ->UUID ->MultiIterator cons ->HashSetIter floats pos? fnil merge-with nthrest -find sequential? m3-mix-H1 ->TransientArrayMap prim-seq shuffle hash-keyword find alength bit-xor ->IndexedSeq unsigned-bit-shift-right neg? -remove-method ->StringIter js-invoke ->List m3-mix-K1 unchecked-float undefined? reduced? apply-to disj! -lookup float? booleans ->ArrayList int-array set? iterable? cat ->ES6EntriesIterator -pr-writer flush set-from-indexed-seq take-while vary-meta is_proto_ <= conj! -pop repeatedly zipmap reset-vals! -remove-watch remove ->BitmapIndexedNode * re-pattern min -persistent! -nth pop! chunk-append prn-str reversible? -realized? -add-watch -deref-with-timeout conj -sorted-seq transduce -swap! js-delete truth_ array-index-of ->MultiFn key->js compare-and-set! array-seq interleave print-map map? get identity into long double volatile? -key nfirst meta -kv-reduce bit-and-not var? -comparator unchecked-add-int hash-ordered-coll reset-meta! ->KeySeq cycle -deref empty? short -clj->js -chunked-first filterv ->TaggedLiteral hash quot ns-interns* unchecked-double ->ChunkedCons ranged-iterator key longs not= set-print-err-fn! string? uri? es6-iterator pr-str-with-opts ->RecordIter ->Symbol unchecked-multiply-int chunk-rest remove-all-methods trampoline double? vec -notify-watches int ->ValSeq rand second find-ns-obj hash-combine > -name replace int? ->Subvec associative? unchecked-int js-keys inst-ms* keyword? array-iter force group-by -rseq prn default-dispatch-val ->Atom unchecked-multiply even? es6-iterator-seq unchecked-dec persistent-array-map-seq tagged-literal? double-array create-ns ->EmptyList spread rseq ex-cause ex-message ->NodeIterator string-print float pr-str es6-set-entries-iterator concat -methods symbol to-array-2d ExceptionInfo mod pop -entry-key dissoc! reductions indexed? - -equiv ->RangeIterator ->ArrayNode assoc! hash-set reduce-kv reset! name ->RedNode ffirst ->ArrayNodeIterator sorted-set ->PersistentTreeMap counted? tagged-literal println assoc-in bit-test ->Namespace ->PersistentHashMap memoize alter-meta! ->StringBufferWriter zero? simple-keyword? -assoc-n unchecked-dec-int persistent! set-print-fn! nnext add-watch not-every? rem ifind? ->t_cljs$core8988 ->HashMapIter ->NodeSeq some ->Box neg-int? drop js-obj nth sorted? nil? split-at prn-str-with-opts random-sample select-keys bit-and bounded-count update find-macros-ns list* ->Keyword update-in prefer-method ensure-reduced ->PersistentArrayMap instance? mix-collection-hash re-find run! val unchecked-add transformer-iterator not -vreset! with-meta unreduced record? type identical? -namespace unchecked-divide-int ns-name max-key ->PersistentTreeSet ->ChunkBuffer hash-string -prefers set-validator! ident? -meta -dispatch-fn ->IndexedSeqIterator -add-method swap! vals -chunked-next unchecked-subtract ->SeqIter sorted-set-by cloneable? qualified-ident? hash-string* key-test -reset true? array -peek empty remove-method volatile! / bit-or m3-fmix vector >= ->TransientHashSet drop-last ->ArrayIter object? ->ArrayNodeSeq not-empty distinct partition ->Many ->Single bit-flip long-array descendants imul ->Delay merge js-mod integer? mapv infinite? partition-all partition-by ->LazySeq equiv-map ->Volatile object-array derive seq-iter ->Empty special-symbol? ancestors subseq gensym -next ->HashCollisionNode delay? flatten -dissoc doubles halt-when -contains-key? remove-watch ex-info ifn? ->PersistentQueue nat-int? subvec -pop! partial chunked-seq? replicate min-key reduced re-matches array-map unchecked-byte ->ChunkedSeq every-pred keys missing-protocol ->t_cljs$core10492 load-file distinct? pos-int? unchecked-short ->Range ->MapEntry methods odd? ->ArrayChunk -get-method ->Var frequencies reduceable? rsubseq inc type->str get-method uuid? es6-entries-iterator bit-clear filter ->PersistentTreeMapSeq -assoc-n! list + split-with ->VectorNode aset int-rotate-left keyword ->Cons chars str next pr-seq-writer regexp? hash-map underive -reset! -rest nil-iter false? ints some-fn to-array list? array? simple-ident? clone demunge bit-not byte max == parents count -disjoin! ->TransientHashMap sorted-map-by apply add-to-string-hash-cache clj->js ->TransientVector interpose ->BlackNode deref assoc transient -disjoin chunk-cons comparator print-prefix-map sorted-map drop-while realized? compare complement -assoc! string-iter -key->js sequence constantly ->RangedIterator chunked-seq make-array shorts ->RSeq enable-console-print! -flush completing unchecked-negate-int ->PersistentVector hash-unordered-coll repeat unchecked-inc nthnext get-validator number? -conj! ->PersistentArrayMapSeq chunk-next print-str not-any? into-array -hash qualified-symbol? -dissoc! ->Reduced chunk-buffer seqable? symbol? m3-hash-unencoded-chars unchecked-char system-time -invoke coll? get-in fnext -val bytes ->ObjMap -seq])
  (:require 
   [reagent.core :as r]
   [cljs.core :as c]))

;; preliminaries

(c/enable-console-print!)

(c/defn on-js-reload [])

;; (defonce show-atom (r/atom {}))

;; (c/defn show [v]
;;   (c/swap! show-atom (c/fn [x] v)))

;; (def ^{:private true :doc "list of all public funcions in cljs.core"} list-cljs-fn
;;   (c/map (c/fn [[k _]] k) 
;;          (c/filter (c/fn [[_ v]] (c/-> v c/meta :arglists c/empty? c/not))
;;                    (c/ns-publics 'cljs.core))))


;; start with defining objects on cljs level

(c/defn ^{:doc "is object?"} cljs-obj? [a]
  (if (:fct/obj? (c/meta a))
    true
    false))

(c/defn ^{:doc "wrappint into an object"} cljs-obj [a]
  (if (cljs-obj? a)
    a
    (c/with-meta (r/atom a) {:fct/obj? true})))


(c/defn ^{:doc "wrapping into an object"} create-branch-obj [key value]
  (c/loop [a (if (cljs-obj? value)
               value
               (cljs-obj [value]))

           key key]

    (if (c/vector? key)

      (if (c/empty? key)
        a
        (c/let [k (c/peek key)]
          (recur (cljs-obj (c/hash-map k a))
                 (c/into [] (c/pop key)))))
      
      (cljs-obj (c/hash-map key a)))))


(c/defn ^{:doc "deref which works on nil"} nil-deref [a]
  (if (c/nil? a)
    nil
    (c/deref a)))

(c/defn ^{:doc "is primitive?"} prim? [a]
  (c/and (cljs-obj? a)
         (c/vector? (nil-deref a))))

;; (def ^{:private true} ex-prim?
;;   (c/println (prim? 5) (prim? (cljs-obj [5]))))

(c/defn ^{:doc "get key in object"} cljs-this [a key]

  (c/loop [a a key key]
    (if (c/vector? key)
      
      (if (c/empty? key)

        (if (prim? a)
          (c/first (nil-deref a))
          (nil-deref a))

        (c/let [[k & r] key]
          (recur (c/get (nil-deref a) k) (c/into [] r))))
      
      (c/let [b (c/get (nil-deref a) key)]
        (if (prim? b)
          
          (c/first (nil-deref b))
          
          (nil-deref b))))))

(def ^{:private true} ex1-cljs-this
  (c/let [a (create-branch-obj [:a] {:x 7})
          key [:a]]
    (cljs-this a key)))

(def ^{:private true} ex2-cljs-this
  (c/let [a (create-branch-obj [:a] {:x 7})
          key []]
    (cljs-this a key)))

;(c/println (c/str ex2-cljs-this))

(c/defn ^{:doc "get key in object"} cljs-this-obj [a key]
  
  (c/loop [a a key key]
    (if (c/vector? key)
      
      (if (c/empty? key)
        
        a
        
        (c/let [[k & r] key]
          (recur (c/get (nil-deref a) k) (c/into [] r))))
      
      (c/get (nil-deref a) key))))

(def ^{:private true} ex-cljs-this-obj
  (c/let [a (create-branch-obj [:x :z] 4)
          key [:x :z]]
    (cljs-this-obj a key)))

;(c/println (c/str (nil-deref ex-cljs-this-obj)))

(c/defn ^{:doc "returns a vector with first entry the keys with non-nil entries and second entry the keys with nil entries"}
  cljs-nil-in-obj [a key]
  
  (if (c/vector? key)

    (c/loop [a a
             key key
             non-nil-key []]
      (if (c/empty? key)
        
        [non-nil-key []]

        (c/let [[k & r] key
                b (c/get (nil-deref a) k)]       
          (if b
            (recur b (c/into [] r) (c/conj non-nil-key k))
            [non-nil-key  key]))))

    (if (c/get (nil-deref a) key)
      [[key] []]
      [[] [key]])))

(def ^{:private true} ex-cljs-nil-in-obj
  (c/let [a (create-branch-obj [:a] 4)
          key :b]
    (cljs-nil-in-obj a key)))

(c/defn ^{:doc "set key in object to a value"} cljs-set [a key value]

  (if key
    
    (c/let [[k nk] (cljs-nil-in-obj a key)
            link (cljs-this-obj a k)]
      (c/cond
        
        (c/empty? k) (if (prim? a)
                       ;; replacing when primitive
                       (c/swap! a (c/fn [x] (c/assoc {} (c/first nk) (create-branch-obj
                                                                      (c/into [] (c/rest nk)) value))))
                       ;; attaching when object
                       (c/swap! a c/assoc (c/first nk) (create-branch-obj
                                                        (c/into [] (c/rest nk)) value)))
        
        (c/empty? nk) (if (cljs-obj? value)
                        ;; attaching when value is an object
                        (c/swap! (cljs-this-obj a (c/pop k)) c/assoc (c/peek k) value)
                        ;; wrapping in vector to make it a primitive when value is not an object
                        (c/swap! (cljs-this-obj a (c/pop k)) c/assoc (c/peek k) (cljs-obj (c/vector value))))
        
        :else  (if (prim? link)
                 ;; replacing when primitive
                 (c/swap! link (c/fn [x] (c/assoc {} (c/first nk) (create-branch-obj
                                                                   (c/into [] (c/rest nk)) value))))
                 ;; attaching when object
                 (c/swap! link c/assoc (c/first nk) (create-branch-obj
                                                     (c/into [] (c/rest nk)) value)))))
    
    nil))

;; (def ^{:private true} ex1-cljs-set
;;   (c/let [a (create-branch-obj [:a :d] 10)
;;           key [:a :d]]
;;     (do (cljs-set a key 5)
;;         a)))

;; (c/println (cljs-this ex1-cljs-set [:a :d]))

(def ^{:private true} ex2-cljs-set
  (c/let [a (create-branch-obj [:a] 1)
          do (cljs-set a :b 0)]
    (c/loop [r (c/range 101)]
      (if r
        (c/let [x (cljs-this a :a)
                y (cljs-this a :b)]
          (do (cljs-set a :b x)
              (cljs-set a :a y)
              (recur (c/next r))))
        a))))
 
;(c/println ex2-cljs-set)

(def ^{:private true} ex3-cljs-set
  (c/let [a (create-branch-obj [:a] 1)
          do (cljs-set a :b 0)]
    (c/loop [r (c/range 1000)]
      (if r
        (c/let [x (cljs-this a :a)
                y (cljs-this a :b)
                do (cljs-set a :b x)
                do (cljs-set a :a y)]             
          (recur (c/next r)))
        a))))

;(c/println (c/str (cljs-this ex3-cljs-set :a) " and " (cljs-this ex3-cljs-set :b)))


(c/defn ^{:doc "merge for cljs-obj?"} obj-merge
  [& ^{:doc "cljs-obj?"} args]

  (c/let [non-nil-args (c/filter #(-> % c/nil? c/not)
                                 args)]
    
    (c/cond (c/empty? non-nil-args) nil
          
            (-> (c/first non-nil-args) prim?) (c/first non-nil-args)
                  
            :else (c/let [args (c/filter #(-> % nil-deref c/map?) non-nil-args)
                          [the-first] args
                          keys (c/into '() (c/into #{} (c/apply c/concat (c/map #(-> % nil-deref c/keys)
                                                                                args))))
                          find-all (c/fn [k] (c/map #(-> % nil-deref (c/get k))
                                                    args))]
                    
                    (c/loop [keys keys]
                      (if keys
                        
                        (c/let [[k] keys
                                do (c/swap! the-first c/assoc k (c/apply obj-merge (find-all k)))]
                          (recur (c/next keys)))
                        
                        the-first))))))


(def ^{:private true} ex-obj-merge
  (c/let [a (create-branch-obj [:a :d] 10)
          b (create-branch-obj [:a :d] 5)]
    (do (cljs-this (obj-merge a b) []))))

;(c/println ex-obj-merge)

;; (def ^{:private true} ex2-obj-merge
;;   (c/let [a (create-branch-obj [:a] {:b 6})
;;           b (create-branch-obj [:a :b] 5)]
;;     (do (cljs-this (obj-merge a b) []))))

;(c/println ex2-obj-merge)

;; (def ^{:private true} ex3-obj-merge
;;   (c/let [a (create-branch-obj [:a] 10)
;;           b (create-branch-obj [:b] 5)]
;;     (do (cljs-this (obj-merge a b) []))))
  
;; (c/println ex3-obj-merge)

;; (def ^{:private true} ex4-obj-merge
;;   (c/let [a (create-branch-obj [:a] 10)
;;           b (cljs-obj {})]
;;     (cljs-this-obj (obj-merge a b) [])))
  
;; (c/println ex4-obj-merge)

(c/defn ^{:doc "merging generators"} gen-merge
  [& ^{:doc "generators, that is fns without arguments with values in hash-maps"} gens]
  (c/fn [] (c/apply obj-merge (c/map (c/fn [g] (g))
                                     gens))))



(c/defn ^{:doc "Grab the keys"} obj-keys [m]

  (if (c/or (prim? m) (c/nil? m)) 

    '()
    
    (c/let [m (nil-deref m)
            keys (c/keys m)]
      (c/apply c/concat (c/map (c/fn [k] (c/let [r (c/map (c/fn [v] (c/into [] (c/cons k v)))
                                                          (obj-keys (k m)))]
                                           (if (c/empty? r)
                                             (c/list [k])
                                             r)))
                               keys)))))

(def ^{:private true} ex-obj-keys
  (c/let [a (create-branch-obj [:a :b] 10)
          b (create-branch-obj [:c :d] 5)
          c (create-branch-obj [:c :e] 0)]
    (obj-keys (obj-merge a b c))))

(c/println ex-obj-keys)

;;
;; evaluate to a usual cljs object
;;

(c/defn ^{:doc "is pure fct object?"} fct?*
  [^{:doc "fct object"} object]
  (:fct/? (c/meta object)))

(c/defn ^{:doc "get the interpretation"} simple-ev*
  [^{:doc "fct object"} object
   ^{:doc "satisfies cljs-obj?"} l]

  (if (fct?* object)
    
    ((c/-> object c/meta :fct/inter) l)
    
    (c/cond
      (c/vector? object) (c/into [] (c/map (c/fn [o] (simple-ev* o l))
                                           object))
      (c/map? object) (c/into {} (c/map (c/fn [[k v]] [(simple-ev* k l) (simple-ev* v l)])
                                        object))
      :else object)))

(clojure.core/defn ^{:doc "evaluation of an fct object resulting in a cljs expression"} ev*
  [^{:doc "fct object"} object
   ^{:doc "cljs-obj"} l
   &
   {:keys [^{:doc "key, meta data of the constructed cljs expression corresponding to this key will be evaluated too"} key]
    :or {key :fct/spec}}]
  
  (if (c/keyword? key)
    
    (c/let [cljs-expression (simple-ev* object l)
            m (c/meta cljs-expression)
            meta-obj (key m)]

      (if meta-obj
        (c/with-meta cljs-expression (c/assoc m key (simple-ev* meta-obj l)))
        cljs-expression))

    (simple-ev* object l)))


(c/defn ^{:doc "find info used to set up gen"} show-gen*
  [^{:doc "fct-object"} object]
  
  (if (fct?* object)

    (:fct/gen (c/meta object))

    (c/cond
      
      (c/vector? object) (c/apply gen-merge (c/map (c/fn [o] (show-gen* o))
                                                   object))
      (c/map? object) (c/apply gen-merge (c/map (c/fn [[k v]] (gen-merge (show-gen* k) (show-gen* v)))
                                                object))
      :else (c/fn [] (cljs-obj {})))))


(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning a cljs-obj? (like l in ev*) a cljs expression"} inter
   & {:keys [^{:doc "generators for the variables"} gen]
      :or {gen (c/fn [] (cljs-obj {}))}}]
  
  (c/with-meta
    (c/fn [& args] (construct*
                    (c/fn [l]
                      (c/apply (inter l) (c/map #(ev* % l) args)))
                    :gen (c/apply gen-merge (c/conj (c/map show-gen*
                                                           args)
                                                    gen))))
    {:fct/? true
     :fct/inter inter
     :fct/gen gen}))

(def ^{:private true} ex1-construct*
  (construct* (c/fn [l] (cljs-this l :a)))) 

(c/println (ev* ex1-construct* (create-branch-obj [:a] 101)))

(def ^{:private true} ex2-construct*
  (construct* (c/fn [l] (cljs-this l :a))
              :gen (c/let [o (cljs-obj {})
                           do (cljs-set o :a (c/rand-nth (c/list true false)))]
                     o)))


(c/defn ^{:doc "keys on which the object depends"} deps*
  [^{:doc "fct-object"} object]
  (obj-keys ((show-gen* object))))

;(c/println (deps* ex2-construct*))


(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object"} a]
  
  (c/let [gen ((show-gen* a))]
    (ev* a gen)))
 
;; (def ^{:private true} ex1-gen*
;;   (c/println (c/map (c/fn [x] (gen* (construct* (c/fn [l] (cljs-this l :a))
;;                                               :gen (c/let [o (cljs-obj {})
;;                                                            do (cljs-set o :a (c/rand-nth (c/list true false)))]
;;                                                      o))))
;;                   (c/range 10))))

(c/defn ^{:doc "as ev*, but generates missing keys with"} gev*
  [^{:doc "fct object"} object
   ^{:doc "as in ev*"} l]
  (ev* object (obj-merge l ((show-gen* object)))))

(c/defn ^{:doc "variable construction"} var*
  ([^{:doc "keyword attached to the variable"} key]
   (var* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object"} object]
   (c/let [key (if (c/keyword? key) [key] key)]
     (construct* (c/fn [l] (cljs-this l key))
                 :gen (c/fn [] (c/let [o (cljs-obj {})
                                       do (cljs-set o key (gen* object))]
                                 o))))))

(def ^{:private true} ex1-var* 
  (c/println (gen* (var* [:a :b] (c/rand-int 10)))))

(def ^{:private true} ex2-var* 
  (c/println (gen* (var* :a {:c (c/rand-int 10)}))))


(c/defn ^{:doc "variable key construction"} key*
  ([^{:doc "keyword attached to the variable"} key]
   (key* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object used for generation"} object]
   (c/let [key (if (c/keyword? key) [key] key)]
     (construct* (c/fn [l] key)
                 :gen (c/fn [] (c/let [o (cljs-obj {})
                                       do (cljs-set o key (gen* object))]
                                 o))))))

(def ^{:private true} ex1-key* 
  (c/println (deps* (key* [:a :b] (c/rand-int 10)))))

(def ^{:private true} ex2-key* 
  (c/println (gen* (key* [:a :b] (c/rand-int 10)))))

(c/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn]
  (construct* (c/fn [l] clojure-fn)))

(def ^{:private true} ex1-lift*
  (c/println (gen* ((lift* c/+) (var* :h (c/rand-int 10)) (var* :a (c/rand))))))

(def ^{:doc "converts expressions with vector and hash-maps to fct"} to-fct
  (lift* c/identity))

(def ^{:doc "clojure interop"} call
  (lift* (c/fn [f & a] (c/apply f a))))

(c/defn ^{:doc "creates an fct function f which evaluates the object on the argument of f"} iso*
  [^{:doc "fct object"} object]
  (lift* (c/fn [a] (gev* object a))))

(lift-cljs) ;; lifting all cljs functions 


(def ^{:private true} ex3-var* 
  (c/println  (gen* (list (var* :a (var* :b (rand-int 10)))
                          (var* :c (var* :b (rand-int 10)))
                          (var* :d (var* :b (rand-int 10)))))))


;; have lifted some macros already 
(def ^{:private true} ex2-if 
  (c/println (gen* (fcts.core/if (and true (var* :a (rand-nth '(true false))))
                     (fcts.core/do (println "Doing something...")
                                   (var* :b "Yer!"))       
                     "False?"))))   

;; have defined fn
(def ^{:private true} ex1-fn 
  (c/println (gen* ((fn [] (var* :a (rand-int 100)))))))

(def ^{:private true} ex2-fn 
  (c/println (gen* ((fn [[_ y]] y)
                    (var* :a ["?" 7])))))

(def ^{:private true} ex3-fn 
  (c/println (gen* ((fn [{:keys [c]}] c)
                    (var* :a {:c "r"})))))
  
; have defined let and defn
(def ^{:private true} ex1-let 
  (c/println (gen* (let [{:keys [c]} (var* :a {:c (rand-int 100)})]
                     c))))


;; loops
;;;
;;; loop
;;;

(def ^{:private true} loopn* (c/fn 
                               ([start test iter ret]
                                (c/loop [l start]
                                  (if (test l)
                                    (ret l)
                                    (recur (iter l)))))
                               ([start test iter]
                                (c/loop [l start]
                                  (if (test l)
                                    l
                                    (recur (iter l)))))))

;; (loopn* '(1 2 3)
;;        (c/fn [a] (c/empty? a))
;;        (c/fn [a] (c/rest a)))

(def loopf (lift* loopn*))

(def rec (lift* c/vector))

(def ex-loop (c/println (ev* (loop [x (range 10)
                                    y []]
                               {:test (empty? x)
                                :rec (let [[f] x]
                                       (rec (rest x) (conj y f)))
                                :ret y})
                             {})))

;;
;; rand
;;

(def ^{:private true} string-to-0-99 (c/fn [string]
                                       (c/let [h (c/rem (c/hash string) 100)]
                                         (if (c/>= h 0)
                                           h
                                           (c/+ h 100)))))
 

(def ^{:doc "random function"}
  rand-fn (lift* (c/fn [^{:doc "function without argument generating values for the random function"}  ret-spec]
                   
                   (c/let [ret-samples (c/map (c/fn [x] (ret-spec))
                                              (c/range 100))]
                     (c/fn [& args] (c/let [c (string-to-0-99 (c/pr-str args))]
                                      (c/nth ret-samples c)))))))

;; (def ^{:private true :doc "1. example for rand-fn in ns fct.core"} ex1-rand-fn
;;   (gen* (rand-fn (fn [] (rand-nth '(true false))))))


(def ^{:doc "random collection"} rand-coll
  (lift* (c/fn [^{:doc "list of elements the collection can consist of"} l
                ^{:doc "number of elements in the collection"} i]
           
           (if (c/empty? l)
             '()
             (c/map (c/fn [] (c/rand-nth l)) (c/range i))))))

;; (def ^{:private true :doc "1. example for rand-coll in ns fct.core"} ex1-rand-coll
;;   (gen* (rand-coll '(1 2 3 4) 5)))


;;
;; testing
;;

(c/defn ^{:doc "applies gen* when called on a fct function, otherwise generates arguments and calls function on them"} check*
  [^{:doc "function"} f]
  
  (c/let [m (c/meta f)
          spec-structure (:fct/spec m)]
    
    (c/cond

      (c/or (c/= nil (:fct/? m))
            (c/= spec-structure nil))  nil

      true  (c/cond
              
              (:fct/? m)
              {:args nil
               :ret (gen* f)} ; to be changed to gen*                     
              
              (:fct/fcn? m)
              (if (c/not (c/or (c/= spec-structure {})
                               (c/= spec-structure [])))
                (c/let [a (spec-structure)]
                  {:args a
                   :ret (c/apply f a)})
                
                {:args []
                 :ret (f)})))))

(c/defn gcheck* [f]
  (check* (gen* f)))

(def ex-gcheck (c/println (gcheck* (fn [x] {:gen (fn [] [(var* :a (rand-int 100))])} x))))

(c/defn ^{:doc "runs tests by using check*"} ftest*

  [^{:doc "function"} f
   & {:keys [count-tests] :or {count-tests 1}}]

  (c/let [test-f (c/fn [x c] (c/let [s (c/map (c/fn [i] (check* x))
                                              (c/range c))
                                     [f] s]
                               (if f
                                 (c/map (c/fn [c] (:ret c))
                                        s)
                                 nil)))
          
          f (c/let [m (c/meta f)]
              (c/cond (:fct/fn? m) (:fct/fn m)
                      true f))
          
          first-result (test-f f count-tests)
          inner-test-loop (c/fn [l] (c/loop [l l
                                             ret '()]
                                      (if (c/empty? l)
                                        ret
                                        (c/let [x (c/first l)
                                                y (test-f x 1)]
                                          (if y
                                            (recur (c/rest l) (c/conj ret (c/first y)))
                                            (recur (c/rest l) ret))))))]
    (c/loop [l first-result]
      (if (c/empty? l)
        true
        (recur (inner-test-loop l))))))


;;
;; performance tests
;;


(c/defn test-loop [n f]
  (c/loop [n n]
    (if (c/= n 0)
      "done"
      (do (f)
          (recur (c/dec n))))))

(def term (fn []
            (rand-nth (map (fn [x] (str "term" x))
                           (range 20)))))

(def t (ev* term {}))

(def c-term (c/fn [] 
              (c/rand-nth (c/map (c/fn [x] (c/str "term" x))
                                 (c/range 20)))))

(c-term)

(c/println "Clojure: 50 ms")
(c/time (test-loop 10000 c-term))

(c/println "with gen: 3600 ms")
(c/time (test-loop 10000 t))

(def element (fn [] (map (fn [x] (term))
                         (range (rand-int 10)))))


(def e (ev* element {}))

(def c-element (c/fn [] (c/map (c/fn [x] (c-term))
                               (c/range (c/rand-int 10)))))

(c-element)

(c/println "Clojure: 4 ms")
(c/time (test-loop 10000 c-element))

(c/println "with gen: ~480 ms")
(c/time (test-loop 10000 e))

(def com (c/let [boolean (c/fn [] (c/rand-nth (c/list true false)))
                 some (lift* (ev* (rand-fn boolean) {}))]
           (fn [x y] 
             (if-else (= x y)
                      true
                      (some x y)))))

;; (def c-com (c/let [boolean (fn [] (rand-nth (list true false)))
;;                    some (ev* (rand-fn boolean) {})]
;;              (c/fn [x y] 
;;                (if (c/= x y)
;;                  true
;;                  (some x y)))))

;; (c-com "term1" "term6")

(def c-com (ev* com {}))

(def add-term (fn [e] 
                (if-else (not (empty? e))
                         (let [first-term (first e)
                               type-check  (every? (fn [t] (com first-term t))
                                                   e)]
                           (if-else type-check
                                    first-term
                                    (list com e)))                                             
                         "cannot deal with empty list")))
 
(c/defn tadd [] (ev* (add-term (element))
                     {}))

(def c-add-term (c/fn [e] 
                  (if (c/not (c/empty? e))
                    (c/let [first-term (c/first e)
                            type-check  (c/every? (c/fn [t] (c-com first-term t))
                                                  e)]
                      (if type-check
                        first-term
                        (c/list c-com e)))                                             
                    "cannot deal with empty list")))

(def find-same (fn [t e] 
                 {:same-type (filter (fn [a] (com t a)) e)
                  :remainder (filter (fn [a] (not (com t a))) e)}))

(c/defn fs [] (ev* (find-same (term) (element)) {}))

 ;; (def s (fn [e] {:gen (fn [] (vector (element)))}
 ;;         (add-term (get (find-same (first e) e) :same-type))))

(def c-find-same (c/fn find-same [t e] 
                   (c/hash-map :same-type (c/filter (c/fn [a] (c-com t a)) e)
                               :remainder (c/filter (c/fn [a] (c/not (c-com t a))) e))))


(c/defn c-fs [] (c-find-same (c-term) (c-element)))


(c/println "Clojure: 50 ms")
(c/time (test-loop 10000 c-fs))

(c/println "with gen: 4900 ms")
(c/time (test-loop 10000 fs))




;; test
(defonce show-atom (cljs-obj {}))

;(cljs-set show-atom [:a] 5)

(def g (var* :a (c/rand-int 30)))

(c/defn show-component []
  (gev* [:p g] show-atom))

(c/defn ^:export run []
  (r/render [show-component]
            (js/document.getElementById "app")))

(run)


