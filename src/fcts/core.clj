(ns fcts.core
  (:refer-clojure :exclude [and cond or lazy-seq])
  (:require [clojure.core :as c])) 

(defmacro my [a]
  `(cljs.core/println ~a))

(defmacro ^{:doc "generic lifting of macros"} lift-macro
  [^{:doc "cljs macro"} macro
   & ^{:doc "arguments for the macro"} arg]
  
  (c/let [l (c/gensym 'fct_lift_macro_arg)
          body (c/cons macro (c/reverse (c/loop [a arg
                                                 na '()]
                                          (if a
                                            (c/let [f (c/first a)]
                                              (recur (c/next a) (c/cons (c/list 'fcts.core/ev* f l)  na)))
                                            na))))
          for-gen (c/loop [a arg
                           na '()]
                     (if a
                       (c/let [f (c/first a)]
                         (recur (c/next a) (c/cons (c/list 'fcts.core/show-gen* f)  na)))
                       na))]
    `(fcts.core/construct* (cljs.core/fn [~l]
                             ~body)
                           :gen (fcts.core/gen-merge ~@for-gen))))

(defmacro throw [& args]
  `(fcts.core/lift-macro throw ~@args))

(defmacro if [& args]
  `(fcts.core/lift-macro if ~@args))

;; just a copy
(c/defmacro if-else [& args]
  `(fcts.core/if ~@args))


(c/defmacro cond [& args]
  `(fcts.core/lift-macro c/cond ~@args))


(c/defmacro lazy-seq [& args]
  `(fcts.core/lift-macro c/lazy-seq ~@args))

(c/defmacro and [& args] `(fcts.core/lift-macro cljs.core/and ~@args))

(c/defmacro or [& args] `(fcts.core/lift-macro cljs.core/or ~@args))

(c/defmacro do [& args] `(fcts.core/lift-macro do ~@args))



(c/defn ^{:doc "is the argument admissible?"} adm-arg?
  [^{:doc "string"} x]
  
  (c/let [y (c/apply c/str (c/take 5 x))
          z (c/apply c/str (c/take 7 x))]
    
    (c/not (c/or (c/= y "map__") (c/= y "seq__") (c/= y "vec__") (c/= z "first__")))))

(c/defn ^{:doc "helper function with deconstruction"} ds
  [^{:doc "symbols that appear in function definition as arguments"} args]
  (c/let [arg# (c/gensym 'fctarg__)
          d (c/destructure [args arg#])
          m (c/map (c/fn [[x y]] x)
                   (c/partition 2 d))]
    (c/loop [v (c/into '() (c/into #{} m))
                            r '()
                            rd '()]
                     (if v
                       (c/let [[x] v]
                         (if (adm-arg? (c/str x))
                           
                           (recur (c/next v)
                                  (c/-> r
                                        (c/conj (c/list 'fcts.core/lift* x))
                                        (c/conj x))
                                  (c/-> rd
                                        (c/conj (c/list 'fcts.core/lift* 0))
                                        (c/conj x)))
                           
                           (recur (c/next v) r rd)))
                       
                       [r rd]))))

(c/defmacro fn [& sigs]
  (c/let [[x y o & b] sigs
          ^{:doc "name, used for recursion (optional)"} name (if (c/symbol? x) x nil)
          ^{:doc "args (required)"} args (if name y x)
          ^{:doc "additional options, e.g. {:gen ...} (optional)"} opt
          (if name
            (if (c/and (c/map? o) (c/not (c/empty? b)))
              o
              nil)
            (if (c/and (c/map? y) o)
              y
              nil))
          ^{:doc "body (required)"} body (c/cons 'fcts.core/do (c/cond (c/and name opt) b
                                                                       name (c/cons o b)
                                                                       opt (c/cons o b)
                                                                       (c/nil? o) (c/list y)
                                                                       :else (c/cons y (c/cons o b))))
          ;; start
          opt (if opt opt (if (c/empty? args)
                            {:gen []}
                            {:gen nil}))
          [liftd# liftdummy#] (ds args)
          for-recursion (if name
                          (c/list name (c/list 'fcts.core/lift* name))
                          '())
          for-recursion-dummy (if name
                                (c/list name (c/list 'fcts.core/lift* 1))
                                '())
          name (if name name (c/gensym 'fct__fn__name))]
    

    `(c/let [args-spec# (:gen ~opt)
             dummy# (cljs.core/let [~@for-recursion-dummy]
                      (cljs.core/let [~@liftdummy#]
                        ~body))
             ev-gen# (fcts.core/show-gen* dummy#)
             inter# (cljs.core/fn [l#]  
                      (cljs.core/with-meta

                        (cljs.core/fn ~name ~args
                          (fcts.core/ev*
                           (cljs.core/let [~@for-recursion]
                             (cljs.core/let [~@liftd#]
                               ~body))
                           l#))
                        
                        {:fct/? false :fct/fcn? true :fct/spec args-spec#}))]
        
       (fcts.core/construct* inter#
                             :gen (fcts.core/gen-merge (fcts.core/show-gen* args-spec#)
                                                       ev-gen#)))))


(c/defmacro defn [name & sigs]
  `(def ~name (fcts.core/fn ~name ~@sigs)))

(c/defmacro ^{:doc "usual syntax"}
  let
  
  ([^{:doc "bindings, deconstruction works"} bindings]
   `(fcts.core/let ~bindings nil))
  
  ([^{:doc "bindings, deconstruction works"} bindings
    ^{:doc "the body"} body]
   
   (if (c/empty? bindings)
     body
     (c/let [[args# val#]  bindings
             [liftd# liftdummy#] (ds args#)
             rec# (c/rest (c/rest bindings))]
       
       `(cljs.core/let [dummy# (cljs.core/let [~@liftdummy#] 
                                 (fcts.core/let [~@rec#] ~body)) 
                        
                        gen# (fcts.core/gen-merge (fcts.core/show-gen* ~val#)
                                                  (fcts.core/show-gen* dummy#))
                        
                        inter# (cljs.core/fn [l#]
                                 (cljs.core/let [~args# (fcts.core/ev* ~val# l#)]
                                   (cljs.core/let [~@liftd#] 
                                     (fcts.core/ev* (fcts.core/let [~@rec#] ~body)
                                                    l#))))]
          
          (construct* inter#
                      :gen gen#))))))


(c/defmacro ^{:doc "the loop macro"} loop
  [^{:doc "as in fct.core/let fixing the initial conditions"} bindings
   {:keys [^{:doc "break out of loop condition"} test
           ^{:doc "iteration step, ending with (rec ...) instead of clojure's (recur ...)"} rec
           ^{:doc "return"} ret]}]
  
  (c/let [all# (c/partition 2 bindings)
          args# (c/into [] (c/map c/first all#))
          data# (c/map c/second all#)]
    
    `(fcts.core/loopf ((fcts.core/lift* c/vector) ~@data#)
                      (fcts.core/fn [~args#] ~test)
                      (fcts.core/fn [~args#] ~rec)
                      (fcts.core/fn [~args#] ~ret))))


(defmacro lift-cljs []
  (c/let [to-read (c/loop [l '(c/->PersistentHashSet c/js->clj c/sort-by c/chunk-first c/print-meta? c/m3-hash-int c/pr-str* c/eduction c/tree-seq c/unchecked-remainder-int c/uuid c/seq c/reduce c/find-ns c/contains? c/every? c/->ES6IteratorSeq c/keep-indexed c/->PersistentQueueSeq c/subs c/set c/take-last c/bit-set c/qualified-keyword? c/->Eduction c/->ES6SetEntriesIterator c/-with-meta c/->PersistentArrayMapIterator c/butlast c/unchecked-subtract-int c/-iterator c/take-nth c/first c/native-satisfies? c/seq? c/-sorted-seq-from c/println-str c/inst-ms c/iterate c/-empty c/newline c/-chunked-rest c/write-all c/fn? c/-prefer-method c/-assoc c/doall c/keyword-identical? c/prefers c/-js->clj c/dedupe c/->ES6Iterator c/dissoc c/atom c/bit-shift-right c/-first c/peek c/aget c/-write c/iter c/mk-bound-fn c/last c/-default-dispatch-val c/pr c/namespace c/obj-map c/-conj c/= c/take c/vector? c/boolean c/bit-shift-left c/random-uuid c/any? c/rand-int c/aclone c/vreset! c/chunk c/dec c/->TransformerIterator c/map c/juxt c/->PersistentQueueIter c/< c/test c/rest c/ex-data c/-drop-first c/isa? c/boolean? c/-clone c/munge c/->NeverEquiv c/re-seq c/char? c/make-hierarchy c/-reduce c/-count c/swap-vals! c/keep c/char c/mapcat c/unchecked-long c/some? c/unchecked-negate c/symbol-identical? c/reverse c/inst? c/range c/bit-count c/sort c/->MetaFn c/unchecked-inc-int c/-compare c/map-indexed c/array-list c/rand-nth c/comp c/array-chunk c/dispatch-fn c/bit-shift-right-zero-fill c/-as-transient c/dorun c/pr-sequential-writer c/simple-symbol? c/disj c/->UUID c/->MultiIterator c/cons c/->HashSetIter c/floats c/pos? c/fnil c/merge-with c/nthrest c/-find c/sequential? c/m3-mix-H1 c/->TransientArrayMap c/prim-seq c/shuffle c/hash-keyword c/find c/alength c/bit-xor c/->IndexedSeq c/unsigned-bit-shift-right c/neg? c/-remove-method c/->StringIter c/js-invoke c/->List c/m3-mix-K1 c/unchecked-float c/undefined? c/reduced? c/apply-to c/disj! c/-lookup c/float? c/booleans c/->ArrayList c/int-array c/set? c/iterable? c/cat c/->ES6EntriesIterator c/-pr-writer c/flush c/set-from-indexed-seq c/take-while c/vary-meta c/is_proto_ c/<= c/conj! c/-pop c/repeatedly c/zipmap c/reset-vals! c/-remove-watch c/remove c/->BitmapIndexedNode c/* c/re-pattern c/min c/-persistent! c/-nth c/pop! c/chunk-append c/prn-str c/reversible? c/-realized? c/-add-watch c/-deref-with-timeout c/conj c/-sorted-seq c/transduce c/-swap! c/js-delete c/truth_ c/array-index-of c/->MultiFn c/key->js c/compare-and-set! c/array-seq c/interleave c/print-map c/map? c/get c/identity c/into c/long c/double c/volatile? c/-key c/nfirst c/meta c/-kv-reduce c/bit-and-not c/var? c/-comparator c/unchecked-add-int c/hash-ordered-coll c/reset-meta! c/->KeySeq c/cycle c/-deref c/empty? c/short c/-clj->js c/-chunked-first c/filterv c/->TaggedLiteral c/hash c/quot c/ns-interns* c/unchecked-double c/->ChunkedCons c/ranged-iterator c/key c/longs c/not= c/set-print-err-fn! c/string? c/uri? c/es6-iterator c/pr-str-with-opts c/->RecordIter c/->Symbol c/unchecked-multiply-int c/chunk-rest c/remove-all-methods c/trampoline c/double? c/vec c/-notify-watches c/int c/->ValSeq c/rand c/second c/find-ns-obj c/hash-combine c/> c/-name c/replace c/int? c/->Subvec c/associative? c/unchecked-int c/js-keys c/inst-ms* c/keyword? c/array-iter c/force c/group-by c/-rseq c/prn c/default-dispatch-val c/->Atom c/unchecked-multiply c/even? c/es6-iterator-seq c/unchecked-dec c/persistent-array-map-seq c/tagged-literal? c/double-array c/create-ns c/->EmptyList c/spread c/rseq c/ex-cause c/ex-message c/->NodeIterator c/string-print c/float c/pr-str c/es6-set-entries-iterator c/concat c/-methods c/symbol c/to-array-2d c/ExceptionInfo c/mod c/pop c/-entry-key c/dissoc! c/reductions c/indexed? c/- c/-equiv c/->RangeIterator c/->ArrayNode c/assoc! c/hash-set c/reduce-kv c/reset! c/name c/->RedNode c/ffirst c/->ArrayNodeIterator c/sorted-set c/->PersistentTreeMap c/counted? c/tagged-literal c/println c/assoc-in c/bit-test c/->Namespace c/->PersistentHashMap c/memoize c/alter-meta! c/->StringBufferWriter c/zero? c/simple-keyword? c/-assoc-n c/unchecked-dec-int c/persistent! c/set-print-fn! c/nnext c/add-watch c/not-every? c/rem c/ifind? c/->t_cljs$core8988 c/->HashMapIter c/->NodeSeq c/some c/->Box c/neg-int? c/drop c/js-obj c/nth c/sorted? c/nil? c/split-at c/prn-str-with-opts c/random-sample c/select-keys c/bit-and c/bounded-count c/update c/find-macros-ns c/list* c/->Keyword c/update-in c/prefer-method c/ensure-reduced c/->PersistentArrayMap c/instance? c/mix-collection-hash c/re-find c/run! c/val c/unchecked-add c/transformer-iterator c/not c/-vreset! c/with-meta c/unreduced c/record? c/type c/identical? c/-namespace c/unchecked-divide-int c/ns-name c/max-key c/->PersistentTreeSet c/->ChunkBuffer c/hash-string c/-prefers c/set-validator! c/ident? c/-meta c/-dispatch-fn c/->IndexedSeqIterator c/-add-method c/swap! c/vals c/-chunked-next c/unchecked-subtract c/->SeqIter c/sorted-set-by c/cloneable? c/qualified-ident? c/hash-string* c/key-test c/-reset c/true? c/array c/-peek c/empty c/remove-method c/volatile! c// c/bit-or c/m3-fmix c/vector c/>= c/->TransientHashSet c/drop-last c/->ArrayIter c/object? c/->ArrayNodeSeq c/not-empty c/distinct c/partition c/->Many c/->Single c/bit-flip c/long-array c/descendants c/imul c/->Delay c/merge c/js-mod c/integer? c/mapv c/infinite? c/partition-all c/partition-by c/->LazySeq c/equiv-map c/->Volatile c/object-array c/derive c/seq-iter c/->Empty c/special-symbol? c/ancestors c/subseq c/gensym c/-next c/->HashCollisionNode c/delay? c/flatten c/-dissoc c/doubles c/halt-when c/-contains-key? c/remove-watch c/ex-info c/ifn? c/->PersistentQueue c/nat-int? c/subvec c/-pop! c/partial c/chunked-seq? c/replicate c/min-key c/reduced c/re-matches c/array-map c/unchecked-byte c/->ChunkedSeq c/every-pred c/keys c/missing-protocol c/->t_cljs$core10492 c/load-file c/distinct? c/pos-int? c/unchecked-short c/->Range c/->MapEntry c/methods c/odd? c/->ArrayChunk c/-get-method c/->Var c/frequencies c/reduceable? c/rsubseq c/inc c/type->str c/get-method c/uuid? c/es6-entries-iterator c/bit-clear c/filter c/->PersistentTreeMapSeq c/-assoc-n! c/list c/+ c/split-with c/->VectorNode c/aset c/int-rotate-left c/keyword c/->Cons c/chars c/str c/next c/pr-seq-writer c/regexp? c/hash-map c/underive c/-reset! c/-rest c/nil-iter c/false? c/ints c/some-fn c/to-array c/list? c/array? c/simple-ident? c/clone c/demunge c/bit-not c/byte c/max c/== c/parents c/count c/-disjoin! c/->TransientHashMap c/sorted-map-by c/apply c/add-to-string-hash-cache c/clj->js c/->TransientVector c/interpose c/->BlackNode c/deref c/assoc c/transient c/-disjoin c/chunk-cons c/comparator c/print-prefix-map c/sorted-map c/drop-while c/realized? c/compare c/complement c/-assoc! c/string-iter c/-key->js c/sequence c/constantly c/->RangedIterator c/chunked-seq c/make-array c/shorts c/->RSeq c/enable-console-print! c/-flush c/completing c/unchecked-negate-int c/->PersistentVector c/hash-unordered-coll c/repeat c/unchecked-inc c/nthnext c/get-validator c/number? c/-conj! c/->PersistentArrayMapSeq c/chunk-next c/print-str c/not-any? c/into-array c/-hash c/qualified-symbol? c/-dissoc! c/->Reduced c/chunk-buffer c/seqable? c/symbol? c/m3-hash-unencoded-chars c/unchecked-char c/system-time c/-invoke c/coll? c/get-in c/fnext c/-val c/bytes c/->ObjMap c/-seq)
                           m '(->PersistentHashSet js->clj sort-by chunk-first print-meta? m3-hash-int pr-str* eduction tree-seq unchecked-remainder-int uuid seq reduce find-ns contains? every? ->ES6IteratorSeq keep-indexed ->PersistentQueueSeq subs set take-last bit-set qualified-keyword? ->Eduction ->ES6SetEntriesIterator -with-meta ->PersistentArrayMapIterator butlast unchecked-subtract-int -iterator take-nth first native-satisfies? seq? -sorted-seq-from println-str inst-ms iterate -empty newline -chunked-rest write-all fn? -prefer-method -assoc doall keyword-identical? prefers -js->clj dedupe ->ES6Iterator dissoc atom bit-shift-right -first peek aget -write iter mk-bound-fn last -default-dispatch-val pr namespace obj-map -conj = take vector? boolean bit-shift-left random-uuid any? rand-int aclone vreset! chunk dec ->TransformerIterator map juxt ->PersistentQueueIter < test rest ex-data -drop-first isa? boolean? -clone munge ->NeverEquiv re-seq char? make-hierarchy -reduce -count swap-vals! keep char mapcat unchecked-long some? unchecked-negate symbol-identical? reverse inst? range bit-count sort ->MetaFn unchecked-inc-int -compare map-indexed array-list rand-nth comp array-chunk dispatch-fn bit-shift-right-zero-fill -as-transient dorun pr-sequential-writer simple-symbol? disj ->UUID ->MultiIterator cons ->HashSetIter floats pos? fnil merge-with nthrest -find sequential? m3-mix-H1 ->TransientArrayMap prim-seq shuffle hash-keyword find alength bit-xor ->IndexedSeq unsigned-bit-shift-right neg? -remove-method ->StringIter js-invoke ->List m3-mix-K1 unchecked-float undefined? reduced? apply-to disj! -lookup float? booleans ->ArrayList int-array set? iterable? cat ->ES6EntriesIterator -pr-writer flush set-from-indexed-seq take-while vary-meta is_proto_ <= conj! -pop repeatedly zipmap reset-vals! -remove-watch remove ->BitmapIndexedNode * re-pattern min -persistent! -nth pop! chunk-append prn-str reversible? -realized? -add-watch -deref-with-timeout conj -sorted-seq transduce -swap! js-delete truth_ array-index-of ->MultiFn key->js compare-and-set! array-seq interleave print-map map? get identity into long double volatile? -key nfirst meta -kv-reduce bit-and-not var? -comparator unchecked-add-int hash-ordered-coll reset-meta! ->KeySeq cycle -deref empty? short -clj->js -chunked-first filterv ->TaggedLiteral hash quot ns-interns* unchecked-double ->ChunkedCons ranged-iterator key longs not= set-print-err-fn! string? uri? es6-iterator pr-str-with-opts ->RecordIter ->Symbol unchecked-multiply-int chunk-rest remove-all-methods trampoline double? vec -notify-watches int ->ValSeq rand second find-ns-obj hash-combine > -name replace int? ->Subvec associative? unchecked-int js-keys inst-ms* keyword? array-iter force group-by -rseq prn default-dispatch-val ->Atom unchecked-multiply even? es6-iterator-seq unchecked-dec persistent-array-map-seq tagged-literal? double-array create-ns ->EmptyList spread rseq ex-cause ex-message ->NodeIterator string-print float pr-str es6-set-entries-iterator concat -methods symbol to-array-2d ExceptionInfo mod pop -entry-key dissoc! reductions indexed? - -equiv ->RangeIterator ->ArrayNode assoc! hash-set reduce-kv reset! name ->RedNode ffirst ->ArrayNodeIterator sorted-set ->PersistentTreeMap counted? tagged-literal println assoc-in bit-test ->Namespace ->PersistentHashMap memoize alter-meta! ->StringBufferWriter zero? simple-keyword? -assoc-n unchecked-dec-int persistent! set-print-fn! nnext add-watch not-every? rem ifind? ->t_cljs$core8988 ->HashMapIter ->NodeSeq some ->Box neg-int? drop js-obj nth sorted? nil? split-at prn-str-with-opts random-sample select-keys bit-and bounded-count update find-macros-ns list* ->Keyword update-in prefer-method ensure-reduced ->PersistentArrayMap instance? mix-collection-hash re-find run! val unchecked-add transformer-iterator not -vreset! with-meta unreduced record? type identical? -namespace unchecked-divide-int ns-name max-key ->PersistentTreeSet ->ChunkBuffer hash-string -prefers set-validator! ident? -meta -dispatch-fn ->IndexedSeqIterator -add-method swap! vals -chunked-next unchecked-subtract ->SeqIter sorted-set-by cloneable? qualified-ident? hash-string* key-test -reset true? array -peek empty remove-method volatile! / bit-or m3-fmix vector >= ->TransientHashSet drop-last ->ArrayIter object? ->ArrayNodeSeq not-empty distinct partition ->Many ->Single bit-flip long-array descendants imul ->Delay merge js-mod integer? mapv infinite? partition-all partition-by ->LazySeq equiv-map ->Volatile object-array derive seq-iter ->Empty special-symbol? ancestors subseq gensym -next ->HashCollisionNode delay? flatten -dissoc doubles halt-when -contains-key? remove-watch ex-info ifn? ->PersistentQueue nat-int? subvec -pop! partial chunked-seq? replicate min-key reduced re-matches array-map unchecked-byte ->ChunkedSeq every-pred keys missing-protocol ->t_cljs$core10492 load-file distinct? pos-int? unchecked-short ->Range ->MapEntry methods odd? ->ArrayChunk -get-method ->Var frequencies reduceable? rsubseq inc type->str get-method uuid? es6-entries-iterator bit-clear filter ->PersistentTreeMapSeq -assoc-n! list + split-with ->VectorNode aset int-rotate-left keyword ->Cons chars str next pr-seq-writer regexp? hash-map underive -reset! -rest nil-iter false? ints some-fn to-array list? array? simple-ident? clone demunge bit-not byte max == parents count -disjoin! ->TransientHashMap sorted-map-by apply add-to-string-hash-cache clj->js ->TransientVector interpose ->BlackNode deref assoc transient -disjoin chunk-cons comparator print-prefix-map sorted-map drop-while realized? compare complement -assoc! string-iter -key->js sequence constantly ->RangedIterator chunked-seq make-array shorts ->RSeq enable-console-print! -flush completing unchecked-negate-int ->PersistentVector hash-unordered-coll repeat unchecked-inc nthnext get-validator number? -conj! ->PersistentArrayMapSeq chunk-next print-str not-any? into-array -hash qualified-symbol? -dissoc! ->Reduced chunk-buffer seqable? symbol? m3-hash-unencoded-chars unchecked-char system-time -invoke coll? get-in fnext -val bytes ->ObjMap -seq)
                           r '()]
                    (if l
                      (recur (c/next l) (c/next m) (c/conj r (c/list 'def (c/first m) (c/list 'fcts.core/lift* (c/first l)))))
                      r))]
    `(do ~@to-read)))
