(ns fcts.core
  (:require-macros [fcts.core :refer [lift-cljs and or lazy-seq cond if-else fn let loop in defn]])
  (:refer-clojure :exclude [->PersistentHashSet js->clj sort-by chunk-first print-meta? m3-hash-int pr-str* eduction tree-seq unchecked-remainder-int uuid seq reduce find-ns contains? every? ->ES6IteratorSeq keep-indexed ->PersistentQueueSeq subs set take-last bit-set qualified-keyword? ->Eduction ->ES6SetEntriesIterator -with-meta ->PersistentArrayMapIterator butlast unchecked-subtract-int -iterator take-nth first native-satisfies? seq? -sorted-seq-from println-str inst-ms iterate -empty newline -chunked-rest write-all fn? -prefer-method -assoc doall keyword-identical? prefers -js->clj dedupe ->ES6Iterator dissoc atom bit-shift-right -first peek aget -write iter mk-bound-fn last -default-dispatch-val pr namespace obj-map -conj = take vector? boolean bit-shift-left random-uuid any? rand-int aclone vreset! chunk dec ->TransformerIterator map juxt ->PersistentQueueIter < test rest ex-data -drop-first isa? boolean? -clone munge ->NeverEquiv re-seq char? make-hierarchy -reduce -count swap-vals! keep char mapcat unchecked-long some? unchecked-negate symbol-identical? reverse inst? range bit-count sort ->MetaFn unchecked-inc-int -compare map-indexed array-list rand-nth comp array-chunk dispatch-fn bit-shift-right-zero-fill -as-transient dorun pr-sequential-writer simple-symbol? disj ->UUID ->MultiIterator cons ->HashSetIter floats pos? fnil merge-with nthrest -find sequential? m3-mix-H1 ->TransientArrayMap prim-seq shuffle hash-keyword find alength bit-xor ->IndexedSeq unsigned-bit-shift-right neg? -remove-method ->StringIter js-invoke ->List m3-mix-K1 unchecked-float undefined? reduced? apply-to disj! -lookup float? booleans ->ArrayList int-array set? iterable? cat ->ES6EntriesIterator -pr-writer flush set-from-indexed-seq take-while vary-meta is_proto_ <= conj! -pop repeatedly zipmap reset-vals! -remove-watch remove ->BitmapIndexedNode * re-pattern min -persistent! -nth pop! chunk-append prn-str reversible? -realized? -add-watch -deref-with-timeout conj -sorted-seq transduce -swap! js-delete truth_ array-index-of ->MultiFn key->js compare-and-set! array-seq interleave print-map map? get identity into long double volatile? -key nfirst meta -kv-reduce bit-and-not var? -comparator unchecked-add-int hash-ordered-coll reset-meta! ->KeySeq cycle -deref empty? short -clj->js -chunked-first filterv ->TaggedLiteral hash quot ns-interns* unchecked-double ->ChunkedCons ranged-iterator key longs not= set-print-err-fn! string? uri? es6-iterator pr-str-with-opts ->RecordIter ->Symbol unchecked-multiply-int chunk-rest remove-all-methods trampoline double? vec -notify-watches int ->ValSeq rand second find-ns-obj hash-combine > -name replace int? ->Subvec associative? unchecked-int js-keys inst-ms* keyword? array-iter force group-by -rseq prn default-dispatch-val ->Atom unchecked-multiply even? es6-iterator-seq unchecked-dec persistent-array-map-seq tagged-literal? double-array create-ns ->EmptyList spread rseq ex-cause ex-message ->NodeIterator string-print float pr-str es6-set-entries-iterator concat -methods symbol to-array-2d ExceptionInfo mod pop -entry-key dissoc! reductions indexed? - -equiv ->RangeIterator ->ArrayNode assoc! hash-set reduce-kv reset! name ->RedNode ffirst ->ArrayNodeIterator sorted-set ->PersistentTreeMap counted? tagged-literal println assoc-in bit-test ->Namespace ->PersistentHashMap memoize alter-meta! ->StringBufferWriter zero? simple-keyword? -assoc-n unchecked-dec-int persistent! set-print-fn! nnext add-watch not-every? rem ifind? ->t_cljs$core8988 ->HashMapIter ->NodeSeq some ->Box neg-int? drop js-obj nth sorted? nil? split-at prn-str-with-opts random-sample select-keys bit-and bounded-count update find-macros-ns list* ->Keyword update-in prefer-method ensure-reduced ->PersistentArrayMap instance? mix-collection-hash re-find run! val unchecked-add transformer-iterator not -vreset! with-meta unreduced record? type identical? -namespace unchecked-divide-int ns-name max-key ->PersistentTreeSet ->ChunkBuffer hash-string -prefers set-validator! ident? -meta -dispatch-fn ->IndexedSeqIterator -add-method swap! vals -chunked-next unchecked-subtract ->SeqIter sorted-set-by cloneable? qualified-ident? hash-string* key-test -reset true? array -peek empty remove-method volatile! / bit-or m3-fmix vector >= ->TransientHashSet drop-last ->ArrayIter object? ->ArrayNodeSeq not-empty distinct partition ->Many ->Single bit-flip long-array descendants imul ->Delay merge js-mod integer? mapv infinite? partition-all partition-by ->LazySeq equiv-map ->Volatile object-array derive seq-iter ->Empty special-symbol? ancestors subseq gensym -next ->HashCollisionNode delay? flatten -dissoc doubles halt-when -contains-key? remove-watch ex-info ifn? ->PersistentQueue nat-int? subvec -pop! partial chunked-seq? replicate min-key reduced re-matches array-map unchecked-byte ->ChunkedSeq every-pred keys missing-protocol ->t_cljs$core10492 load-file distinct? pos-int? unchecked-short ->Range ->MapEntry methods odd? ->ArrayChunk -get-method ->Var frequencies reduceable? rsubseq inc type->str get-method uuid? es6-entries-iterator bit-clear filter ->PersistentTreeMapSeq -assoc-n! list + split-with ->VectorNode aset int-rotate-left keyword ->Cons chars str next pr-seq-writer regexp? hash-map underive -reset! -rest nil-iter false? ints some-fn to-array list? array? simple-ident? clone demunge bit-not byte max == parents count -disjoin! ->TransientHashMap sorted-map-by apply add-to-string-hash-cache clj->js ->TransientVector interpose ->BlackNode deref assoc transient -disjoin chunk-cons comparator print-prefix-map sorted-map drop-while realized? compare complement -assoc! string-iter -key->js sequence constantly ->RangedIterator chunked-seq make-array shorts ->RSeq enable-console-print! -flush completing unchecked-negate-int ->PersistentVector hash-unordered-coll repeat unchecked-inc nthnext get-validator number? -conj! ->PersistentArrayMapSeq chunk-next print-str not-any? into-array -hash qualified-symbol? -dissoc! ->Reduced chunk-buffer seqable? symbol? m3-hash-unencoded-chars unchecked-char system-time -invoke coll? get-in fnext -val bytes ->ObjMap -seq])
  (:require 
   [reagent.core :as r]
   [cljs.core :as c]))
  
;; preliminaries

(c/enable-console-print!)

(c/defn on-js-reload [])


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
    (r/atom a :meta {:fct/obj? true})))


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
  (c/println (c/let [a (create-branch-obj [:a] {:x 7})
                     key []]
               (cljs-this a key))))


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

        ;; case key=[]
        (c/and (c/empty? k) (c/empty? nk)) (if (cljs-obj? value)
                                             
                                             (if (prim? a)
                                               ;; not supported
                                               nil
                                               ;; replacing all keys  
                                               (c/reset! a (cljs-this value [])))

                                             ;; replacing value if not key
                                             (c/reset! a (c/vector value)))
                                             
                                             
        (c/empty? k) (if (prim? a)
                       ;; replacing when primitive
                       (c/swap! a (c/fn [x] (c/assoc {} (c/first nk) (create-branch-obj
                                                                      (c/into [] (c/rest nk)) value))))
                       ;; attaching otherwise
                       (c/swap! a c/assoc (c/first nk) (create-branch-obj
                                                        (c/into [] (c/rest nk)) value)))
        
        (c/empty? nk) (if (cljs-obj? value)
                        ;; attaching when value is an object
                        (c/swap! (cljs-this-obj a (c/pop k)) c/assoc (c/peek k) value)
                        ;; resetting link to [value] to make it primitive when value is not an object
                        (c/reset! link (c/vector value)))
        
        :else  (if (prim? link)
                 ;; replacing when primitive
                 (c/swap! link (c/fn [x] (c/assoc {} (c/first nk) (create-branch-obj
                                                                   (c/into [] (c/rest nk)) value))))
                 ;; attaching when object
                 (c/swap! link c/assoc (c/first nk) (create-branch-obj
                                                     (c/into [] (c/rest nk)) value)))))
    
    nil))

(c/defn ^{:doc "set key in object to a value"} cljs-set-sync [a & args]

  (c/loop [args args]
    (if args
      (do (cljs-set a (c/first args) (c/second args))
          (recur (c/next (c/next args))))
      nil)))

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
 
(c/println ex2-cljs-set)

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

(c/println (c/str (cljs-this ex3-cljs-set :a) " and " (cljs-this ex3-cljs-set :b)))

(def ^{:private true} ex4-cljs-set
  (c/println (c/let [a (create-branch-obj [:a] 1)
                     do (cljs-set a :b 0)
                     f (c/fn [& args]
                         (cljs-set a (c/first args) (c/second args))
                         (cljs-set a (c/nth args 2) (c/nth args 3)))]
               (do (f :a (cljs-this a :b) :b (cljs-this a :a))
                   a))))

(def ^{:private true} ex5-cljs-set
  (c/println (c/let [a (create-branch-obj [:a] 1)
                     do (cljs-set a :b 0)
                     f (c/fn [a] (c/fn [& args]
                                   (c/loop [args args]
                                     (if args
                                       (do (cljs-set a (c/first args) (c/second args))
                                           (recur (c/next (c/next args))))
                                       nil))))]
               (do ((f a) :a (cljs-this a :b) :b (cljs-this a :a))
                   a))))

(def ^{:private true} ex-cljs-set-sync
  (c/println (c/let [a (create-branch-obj [:a] 1)
                     do (cljs-set a :b 0)]
               (do (cljs-set-sync a :a (cljs-this a :b) :b (cljs-this a :a))
                   a))))

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

;; (c/defn ^{:doc "merging generators"} gen-merge
;;   [& ^{:doc "generators, that is fns without arguments with values in hash-maps"} gens]
;;   (c/fn [] (c/apply obj-merge (c/map (c/fn [g] (g))
;;                                      gens))))



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


;; (c/defn ^{:doc "find info used to set up gen"} show-gen*
;;   [^{:doc "fct-object"} object]
  
;;   (if (fct?* object)

;;     (:fct/gen (c/meta object))

;;     (c/cond
      
;;       (c/vector? object) (c/apply gen-merge (c/map (c/fn [o] (show-gen* o))
;;                                                    object))
;;       (c/map? object) (c/apply gen-merge (c/map (c/fn [[k v]] (gen-merge (show-gen* k) (show-gen* v)))
;;                                                 object))
;;       :else (c/fn [] (cljs-obj {})))))


(c/defn ^{:doc "constructs an fct object"} construct*
  [^{:doc "function assigning a cljs-obj? (like l in ev*) a cljs expression"} inter]
  
  (c/with-meta
    (c/fn [& args] (construct*
                    (c/fn [l]
                      (c/apply (inter l) (c/map #(ev* % l) args)))
                    ))  ;;:gen (c/apply gen-merge (c/conj (c/map show-gen* args gen))
    {:fct/? true
     :fct/inter inter
     ;:fct/gen gen
     }))

(def ^{:private true} ex1-construct*
  (construct* (c/fn [l] (cljs-this l :a)))) 

(c/println (ev* ex1-construct* (create-branch-obj [:a] 101)))

(c/defn ^{:doc "constructs an fct object"} construct-doall*
  [^{:doc "function assigning a cljs-obj? (like l in ev*) a cljs expression"} inter]
  
  (c/with-meta
    (c/fn [& args] (construct*
                    (c/fn [l]
                      (c/apply (inter l) (c/doall (c/map #(ev* % l) args))))
                    ))  ;;:gen (c/apply gen-merge (c/conj (c/map show-gen* args gen))
    {:fct/? true
     :fct/inter inter
     ;:fct/gen gen
     }))


(c/defn ^{:doc "lifting clojure functions"} lift*
  [^{:doc "clojure function (not macro)"} clojure-fn]
  (construct* (c/fn [l] clojure-fn)))


(def ^{:doc "converts expressions with vector and hash-maps to fct"} to-fct
  (lift* c/identity))

(def ^{:doc "clojure interop"} call
  (lift* (c/fn [f & a] (c/apply f a))))

(c/defn ^{:doc "creates an fct function f which evaluates the object on the argument of f"} iso*
  [^{:doc "fct object"} object]
  (lift* (c/fn [a] (ev* object a))))

(lift-cljs) ;; lifting all cljs functions 


(def this (construct* (c/fn [l] l)))

(def find-in (construct* (c/fn [l]
                           (c/fn
                             ([key] (cljs-this l key))
                             ([state key] (cljs-this state key))))))

(def obj-find-in (construct* (c/fn [l]
                               (c/fn
                                 ([key] (cljs-this-obj l key))
                                 ([state key] (cljs-this-obj state key))))))

(def ex-find-in (c/println (ev* (find-in :a) (create-branch-obj [:a] 11))))

(def ex-obj-find-in (c/println (ev* (obj-find-in :a) (create-branch-obj [:a] 111))))

(def reset (construct-doall* (c/fn [l]
                               (c/fn [f & args]
                                 (c/cond
                                   
                                   (c/keyword? f) (c/apply cljs-set-sync (c/cons l (c/cons f args)))
                                   
                                   (c/vector? f) (c/apply cljs-set-sync (c/cons l (c/cons f args)))
                                   
                                   :else (c/apply cljs-set-sync (c/cons f args)))))))



(def ex-reset (c/println (ev* (fcts.core/do (reset :a "1001")
                                            (find-in :a))
                              (cljs-obj {}))))

(def ex2-reset (c/println (ev* (c/let [state (cljs-obj {})]
                                 (fcts.core/do
                                   (reset state :a "10001" :b "444")
                                   (find-in state :b)))
                               (cljs-obj {}))))

(def ex-reset-state-b (cljs-obj {}))
(def ex-reset-state-a (cljs-obj {}))

(def ex3-reset (c/println (do (ev* (reset :b "b") ex-reset-state-b)
                              (ev* (reset :a (obj-find-in ex-reset-state-b :b)) ex-reset-state-a)
                              (ev* (reset :a "a") ex-reset-state-a)
                              (ev* (find-in :b) ex-reset-state-b))))

(def ex4-reset (c/println (ev* (in (reset [] (create-branch-obj [:A] "A"))
                                   (c/let [c (cljs-obj {})]
                                     (in (reset c [] (obj-find-in []))
                                         (reset [:A] "AA")
                                         (find-in c []))))
                               (cljs-obj {}))))

(def ^{:private true} ex5-reset
  (c/println (ev*  (in (reset :a 0)
                       (reset :b 1)
                       (reset :b (find-in :a) :a (find-in :b))
                       {:a (find-in :a) :b (find-in :b)})
                   (cljs-obj {}))))


(c/defn ^{:doc "replaces in the interpretation for the fct object the global state (l) with the state generated by the state generator"} on-state*
  ([^{:doc "fct object"} object
    ^{:doc "state generator, fct function with state as argument listing all the side effects"} f]
   
   (construct* (c/fn [l] (c/let [o (cljs-obj {})
                                 do (ev* (f o) l)]

                           (ev* object o)))))

  ([^{:doc "fct object"} object
    ^{:doc "state generator, fct function with state as argument listing all the side effects"} f
    ^{:doc "key on which the local state passed to object should be synchronised with"} key]
   
   (construct* (c/fn [l] (c/let [o (cljs-set l key (cljs-obj {}))   ;deleting whatever is at key 
                                 do (ev* (f (obj-find-in key)) l)]

                           (ev* object (cljs-this-obj l key)))))))

(def ex-on-state* (c/println (ev* (on-state* (find-in :a)
                                             (fn [state] (reset state :a 5)))
                                  (cljs-obj {}))))

(def ex-state (cljs-obj {}))

(def ex2-on-state* (c/println (ev* (on-state* (in (println "first" (find-in :a))
                                                  (reset :a 0)
                                                  (println "second "(find-in :a)))
                                              
                                              (fn [state]
                                                (reset :b 55)
                                                (reset state :a (obj-find-in :b)))

                                              :local-state)
                                   
                                   ex-state)))

(c/println (ev* (find-in []) ex-state))




(clojure.core/defn ^{:doc "generates a witness"} gen*
  [^{:doc "fct object"} a]
  
  (ev* a (cljs-obj {})))


(c/defn ^{:doc "variable construction"} init*
    
  ([key] (init* key nil))

  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object"} object]

   (c/let [key (if (c/keyword? key) [key] key)]

    (construct* (c/fn [l]
                  (c/let [v (cljs-this l key)
                          e (ev* object l)
                          r (if v
                              nil
                              (cljs-set l key e))]
                          (cljs-this l key)))))))
  
  
(def ^{:private true} ex1-init* 
  (c/println (gen* (c/let [a (init* :a (c/rand-int 10))
                           b (init* [:b] a)]
                     (list a b))))) 

;; consistency 
(def ^{:private true} ex2-init* 
  (c/println (gen* (c/let [a 
                           (init* [:a] (c/rand-int 10))
                           b 
                           (init* [:b] a)
                           c 
                           (init* [:c] a)]
                     (list a b c)))))


(c/defn ^{:doc "variable key construction"} key*
  ([^{:doc "keyword attached to the variable"} key]
   (key* key nil))
  ([^{:doc "keyword attached to the variable"} key
    ^{:doc "fct object used for generation"} object]
   (c/let [key (if (c/keyword? key) [key] key)]
     (construct* (c/fn [l] key)
                 ;:gen (c/fn [] (c/let [o (cljs-obj {}) do (cljs-set o key (gen* object))] o))
                 ))))


;;have lifted some macros already 
(def ^{:private true} ex2-if 
  (c/println (gen* (fcts.core/if (and true 
                                      (init* :a (rand-nth '(true false))))
                     (fcts.core/do (println "Doing something...")
                                   (init* :b "Yer"))       
                     "False?"))))     

;; have defined fn
(def ^{:private true} ex1-fn 
  (c/println (gen* ((fn [] 
                      (init* :a (rand-int 100)))))))

(def ^{:private true} ex2-fn 
  (c/println (gen* ((fn [[_ y]] y)
                    (init* :a ["?" 7])))))

;; (def ^{:private true} ex3-fn 
;;   (c/println (gen* ((fn [{:keys [c]}] c)
;;                     (init* :a {:c "r"})))))
  
;; ; have defined let and defn
;; (def ^{:private true} ex1-let 
;;   (c/println (gen* (let [{:keys [c]} (init* :a {:c (rand-int 100)})]
;;                      c))))


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
               :ret (ev* f (cljs-obj {}))       ;(gen* f)
               } ; to be changed to gen*                     
              
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

;;(def ex-gcheck (c/println (gcheck* (fn [x] {:gen (fn [] [(init* :a (rand-int 100))])} x))))

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


;; (c/defn test-loop [n f]
;;   (c/loop [n n]
;;     (if (c/= n 0)
;;       "done"
;;       (do (f)
;;           (recur (c/dec n))))))

;; (def term (fn []
;;             (rand-nth (map (fn [x] (str "term" x))
;;                            (range 20)))))

;; (def t (ev* term {}))

;; (def c-term (c/fn [] 
;;               (c/rand-nth (c/map (c/fn [x] (c/str "term" x))
;;                                  (c/range 20)))))

;; (c-term)

;; (c/println "Clojure: 50 ms")
;; (c/time (test-loop 10000 c-term))

;; (c/println "with gen: 3600 ms")
;; (c/time (test-loop 10000 t))

;; (def element (fn [] (map (fn [x] (term))
;;                          (range (rand-int 10)))))


;; (def e (ev* element {}))

;; (def c-element (c/fn [] (c/map (c/fn [x] (c-term))
;;                                (c/range (c/rand-int 10)))))

;; (c-element)

;; (c/println "Clojure: 4 ms")
;; (c/time (test-loop 10000 c-element))

;; (c/println "with gen: ~480 ms")
;; (c/time (test-loop 10000 e))

;; (def com (c/let [boolean (c/fn [] (c/rand-nth (c/list true false)))
;;                  some (lift* (ev* (rand-fn boolean) {}))]
;;            (fn [x y] 
;;              (if-else (= x y)
;;                       true
;;                       (some x y)))))

;; ;; (def c-com (c/let [boolean (fn [] (rand-nth (list true false)))
;; ;;                    some (ev* (rand-fn boolean) {})]
;; ;;              (c/fn [x y] 
;; ;;                (if (c/= x y)
;; ;;                  true
;; ;;                  (some x y)))))

;; ;; (c-com "term1" "term6")

;; (def c-com (ev* com {}))

;; (def add-term (fn [e] 
;;                 (if-else (not (empty? e))
;;                          (let [first-term (first e)
;;                                type-check  (every? (fn [t] (com first-term t))
;;                                                    e)]
;;                            (if-else type-check
;;                                     first-term
;;                                     (list com e)))                                             
;;                          "cannot deal with empty list")))
 
;; (c/defn tadd [] (ev* (add-term (element))
;;                      {}))

;; (def c-add-term (c/fn [e] 
;;                   (if (c/not (c/empty? e))
;;                     (c/let [first-term (c/first e)
;;                             type-check  (c/every? (c/fn [t] (c-com first-term t))
;;                                                   e)]
;;                       (if type-check
;;                         first-term
;;                         (c/list c-com e)))                                             
;;                     "cannot deal with empty list")))

;; (def find-same (fn [t e] 
;;                  {:same-type (filter (fn [a] (com t a)) e)
;;                   :remainder (filter (fn [a] (not (com t a))) e)}))

;; (c/defn fs [] (ev* (find-same (term) (element)) {}))

;;  ;; (def s (fn [e] {:gen (fn [] (vector (element)))}
;;  ;;         (add-term (get (find-same (first e) e) :same-type))))

;; (def c-find-same (c/fn find-same [t e] 
;;                    (c/hash-map :same-type (c/filter (c/fn [a] (c-com t a)) e)
;;                                :remainder (c/filter (c/fn [a] (c/not (c-com t a))) e))))


;; (c/defn c-fs [] (c-find-same (c-term) (c-element)))


;; (c/println "Clojure: 50 ms")
;; (c/time (test-loop 10000 c-fs))

;; (c/println "with gen: 4900 ms")
;; (c/time (test-loop 10000 fs))


;; (def simplify (fn [e] 
;;                 (loop [x e
;;                        r []]
;;                   {:test (empty? x)
;;                    :rec (let [[t] x
;;                               {:keys [remainder same-type]} (find-same t x)
;;                               sum  (add-term same-type)
;;                               new-y (conj r sum)]
;;                           (rec remainder new-y))
;;                    :ret r})))


;; (c/defn tsimp [] (ev* (simplify (element)) {}))


;; (def c-simplify (c/fn [e] 
;;                   (c/loop [x e
;;                            r []]
;;                     (if (c/empty? x)
;;                       r
;;                       (c/let [[t] x
;;                               {:keys [remainder same-type]} (c-find-same t x)
;;                               sum  (c-add-term same-type)
;;                               new-y (c/conj r sum)]
;;                         (recur remainder new-y))))))

;; (c-simplify (c-element))

;; (c/defn c-tsimp [] (c-simplify (c-element)))

;; ;; we could also use (s/coll-of element) 
;; (def add (fn [& elements] 
;;            (simplify (apply concat elements))))


;; (def c-add (c/fn [& elements] 
;;              (c-simplify (c/apply c/concat elements))))

;; (c-add (c-element) (c-element))

;; ;; ;; multiplication of terms
;; (def mult-term (lift* ((ev* rand-fn {}) (ev* element {}))))



;; (def c-mult-term (ev* mult-term {}))



;; (def mult (fn [& elements] 

;;             (c/let [simple-mult1 (fn [t e] 
;;                                  (loop [e e r []]
;;                                    {:test (empty? e)
;;                                     :rec (let [[s] e]
;;                                            (rec (rest e) (concat (mult-term t s) r)))
;;                                     :ret (simplify r)}))
;;                     simple-mult  (fn [e1 e2] 
;;                                    (loop [e1 e1 r []]
;;                                      {:test (empty? e1)
;;                                       :rec (let [[t] e1]
;;                                              (rec (rest e1)
;;                                                   (concat (simple-mult1 t e2)
;;                                                           r)))
;;                                       :ret (simplify r)}))]
              
;;               (if-else (empty? elements)
;;                        []
;;                        (loop [elements (rest elements)
;;                               r (first elements)]
;;                          {:test (empty? elements)
;;                           :rec (let [[e] elements]
;;                                  (rec (rest elements) (simple-mult r e)))
;;                           :ret (simplify r)})))))


;; (c/defn mt [] (ev* (mult (element) (element) (element))
;;                    {}))



;; (def c-mult (c/fn [& elements] 

;;               (c/let [simple-mult1 (c/fn [t e] 
;;                                      (c/loop [e e r []]
;;                                        (if  (c/empty? e)
;;                                          (c-simplify r)
;;                                          (c/let [[s] e]
;;                                            (recur (c/rest e) (c/concat (c-mult-term t s) r))))))
;;                   simple-mult  (c/fn [e1 e2] 
;;                                  (c/loop [e1 e1 r []]
;;                                    (if (c/empty? e1)
;;                                      (c-simplify r)
;;                                      (c/let [[t] e1]
;;                                        (recur (c/rest e1)
;;                                               (c/concat (simple-mult1 t e2)
;;                                                         r))))))]
              
;;                 (if (c/empty? elements)
;;                   []
;;                   (c/loop [elements (c/rest elements)
;;                            r (c/first elements)]
;;                     (if (c/empty? elements)
;;                       (c-simplify r)
;;                       (c/let [[e] elements]
;;                         (recur (c/rest elements) (simple-mult r e)))))))))


;; (c/defn c-mt [] (c-mult (c-element) (c-element) (c-element)))

;; (c/println  "Clojure:")
;; (c/time (test-loop 100 c-mt))

;; (c/println  "Best: 1400 ms")
;; (c/time (test-loop 100 mt))

      
;; test

(defonce app-state (cljs-obj {}))

(c/defn another-timing-wrapper [f]
  (c/let [now #(.now js/Date)
          start #(c/swap! app-state c/assoc :start-time (now))
          stop #(c/swap! app-state c/assoc :render-time (c/- (now) (:start-time @app-state)))
          timed-f (c/with-meta f
                    {:component-will-mount start
                     :component-will-update start
                     :component-did-mount stop
                     :component-did-update stop})]
    (c/fn []
      [:div 
       [:p [:em "render time: " (:render-time @app-state) "ms"]]
       [timed-f]])))


(c/defn timing-wrapper [f]
  (c/let [start-time (r/atom nil)
          render-time (r/atom nil)
          now #(.now js/Date)
          start #(c/reset! start-time (now))
          stop #(c/reset! render-time (c/- (now) @start-time))
          timed-f (c/with-meta f
                    {:component-will-mount start
                     :component-will-update start
                     :component-did-mount stop
                     :component-did-update stop})]
    (c/fn []
      [:div 
       [:p [:em "render time: " @render-time "ms"]]
       [timed-f]])))

(def render-time (init* :render-time "??"))

(defn fct-timing-wrapper [f]
  (c/let [a (init* :render-time "???")]
      (let [now #(.now js/Date)
            start (fn [] (reset :start-time (now)))
            stop  (fn [] (reset :render-time (- (now) (find-in :start-time))))
            timed-f (with-meta f
                      {:component-will-mount start
                       :component-will-update start
                       :component-did-mount stop
                       :component-did-update stop})]
        (fn []
          [:div 
           [:p [:em "render time: " a "ms"]]
           [timed-f]]))))

(def click-count (init* :click-count 0))

(def state-ful-with-atom (fn []
                           [:button {:on-click (fn [] (in (println (str click-count))
                                                          (reset :click-count (inc click-count))))}
                            [:p (call (c/fn [click-count] (c/str "I have been clicked " click-count))
                                      click-count)]]))

(def count-atom (r/atom 0))
(def crazy-state (r/atom {}))

;;(defonce click-count (r/atom 0))

;; (c/defn state-ful-with-atom []
;;   [:div {:on-click #(c/swap! click-count c/inc)}
;;    "I have been clicked " @click-count " times."])


(c/defn iter-component [state]
  [:button {:on-click (c/fn [] (do (c/println "Here!")
                                   (c/swap! crazy-state
                                            (c/fn [a] {:component iter-component :count (c/inc (:count a))}))))}
   "???????????????"
   [:p (c/str "We are at: " state)]
   [:div 
    "Click here!"]])

(c/reset! crazy-state {:component iter-component
                       :count 0})

(c/println crazy-state)

(c/defn together []
  [(:component (c/deref crazy-state))
   (:count (c/deref crazy-state))])


(def component (init* :component))

(defn f-iter-component [state]
  [:button {:on-click (fn [] (in (println (= component f-iter-component))
                                 (println (list (meta component) (meta f-iter-component)))
                                 (reset :click-count (inc click-count)
                                        :component f-iter-component)))}
   "???????????????"
   [:p (str "We are at: " state)]
   [:div 
    "Click here!"]])

(defn f-together []
  (in (if-else (= nil component)
               (in (reset :component f-iter-component)
                   (println (meta component))))
      [:div [component
             click-count]
       [:p (str (meta f-iter-component))]
       [:p (str (meta (lift* (c/with-meta  (c/fn [] "??") {:doc "d"}))))]]))

(def re (c/fn ^{:doc "this"} g [] g))

(c/println (c/meta (re)))

(c/defn ^:export run []  
  (r/render (gen* [timing-wrapper f-together])
            (js/document.getElementById "app")))

(run)




;; (defn to-rgb [{:keys [red green blue]}] {:gen (fn [] [{:red (rand-int 100)
;;                                                        :green (rand-int 100)
;;                                                        :blue (rand-int 100)}])}
;;   (c/let [hex (lift* #(.toString % 16))]
;;     (str "#" (hex red) (hex green) (hex blue))))

;; (c/println (gcheck* to-rgb))

;; (defn tweak-color [{:keys [red green blue]}] {:gen (fn [] [{:red (rand-int 100)
;;                                                             :green (rand-int 100)
;;                                                             :blue (rand-int 100)}])}
;;   (c/let [rnd #(-> (js/Math.random) (c/* 256))
;;           tweak (lift* #(-> % (c/+ (rnd)) (c// 2) js/Math.floor))]
;;     {:red (tweak red) :green (tweak green) :blue (tweak blue)}))

;; (c/println (gcheck* tweak-color))

(c/defn to-rgb [{:keys [red green blue]}]
  (c/let [hex #(.toString % 16)]
    (c/str "#" (hex red) (hex green) (hex blue))))

(c/defn tweak-color [{:keys [red green blue]}]
  (c/let [rnd #(-> (js/Math.random) (c/* 256))
          tweak #(-> % (c/+ (rnd)) (c// 2) js/Math.floor)]
    {:red (tweak red) :green (tweak green) :blue (tweak blue)}))

(def base-color (init* :base-color {:red 130 :green 160 :blue 120}))

(def random-colors  (init* :random-colors (repeatedly (fn [] (apply #(-> % tweak-color to-rgb)
                                                                   [base-color])))))
(defn color-choose [color-part]
  [:div.color-slider
   (name color-part) " " 
   [:input {:type "range" :min 0 :max 255
            :value (get base-color color-part)
            :on-change (fn [e]
                         (let [new-bc (assoc base-color
                                             color-part ((lift* #(-> % .-target .-value c/int)) e))]
                           (in (reset :base-color new-bc)
                               (reset :random-colors
                                      (repeatedly (fn [] (apply #(-> % tweak-color to-rgb)
                                                                [new-bc])))))))}]

   (get base-color color-part)])

(def ncolors (init* :ncolors 20))

(defn ncolors-choose []
  [:div.color-slider
   "number of color divs " 
   [:input {:type "range" :min 0 :max 500
            :value ncolors
            :on-change (fn [e] (reset :ncolors ((lift* #(-> % .-target .-value c/int)) e)))}]
   ncolors])

(defn color-plate [color]
  [:div.color-plate
   {:style {:background-color color}}
   "??"])

(defn palette []
  (let [color base-color
        n ncolors]
    [:div
     [:p "base color: "]
     [color-plate (apply to-rgb [color])]
     [:div.color-samples
      [:p n " random matching colors:"]
      (apply (c/fn [color-plate n random-colors]
               (c/map-indexed (c/fn [k v]
                                (c/with-meta [color-plate v]
                                  {:key k}))
                              (c/doall (c/take n random-colors))))
             [color-plate n random-colors])]]))

(defn color-demo []
  (fn []
    [:div
     [:h2 "Matching colors"]
     [color-choose :red]
     [color-choose :green]
     [color-choose :blue]
     [ncolors-choose]
     [timing-wrapper palette]]))


;; (c/defn ^:export run []  
;;   (r/render (gen* [color-demo])
;;             (js/document.getElementById "app")))

;; (run)



