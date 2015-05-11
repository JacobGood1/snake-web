(ns snake-web.imperative-assistance
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :refer [match]]))



(defmacro set-all!
  [object & args]
  (let [format (fn [x] (symbol (str ".-" (apply str (drop 1 (str x))))))]
    `(do
       ~@(for [[x y] (partition 2 args)]
           `(set! ~(list (format x) y) ~object)))));


;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- make-map-destructure
  [map obj-name]
  (let [true-map (eval map)]
    (apply hash-map
           (-> (for [[a b] true-map] [(symbol (str obj-name "-" (.substring (str a) 1))) a])
               flatten
               reverse
               (conj :as map)
               reverse))))


(comment (defmacro def-method
           "name of method followed by a vector of maps, access map information with
           name-property"
           [name maps args & code]
           `(let [morph-objects (map #(make-map-destructure % %) maps)]
              (defn ~name
                ~(vec (concat args)
                      ~@code))))                            ;;TODO maybe fix this crapper

         (def point {:x 1 :y 1 :my-toe "ouch"})

         (def-method square-y [point]
                     (* point-y point-y))

         (square-y point))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; operator precedence for formula macro
(def +precedence-table+ (atom {}))

;; symbol translation for symbols in formula (only supports binary operators)
(def +translation-table+ (atom {}))

(def +highest-precedence+ (atom 0))

(clojure.core/defn defop
  "Define operators for formula macro"
  ([op prec & [trans]]
   (swap! +precedence-table+ assoc op prec)
   (when-not (nil? trans)
     (swap! +translation-table+ assoc op trans))
   (reset! +highest-precedence+ (reduce max (map val @+precedence-table+)))))



(defmacro div
  [args]
  `(/ ~@args))

;; == operators ==
(def ** 1)
(defop '|| 10 'or)
(defop '&& 20 'and)
(defop '== 30 '=)
(defop '!= 30 'not=)
(defop '< 40)
(defop '> 40)
(defop '<= 40)
(defop '>= 40)
(defop 'mod 90 'rem)
(defop '- 60 '-)
(defop '+ 60 '+)
(defop '/ 80 '/)
(defop (symbol "/") 80 '/)
(defop '* 80 '*)

(defop '** 100 '(.pow js/Math))



(clojure.core/defn- operator?
  "Check if is valid operator"
  ([sym]
   (not (nil? (get @+precedence-table+ sym)))))

(clojure.core/defn- find-lowest-precedence
  "find the operator with lowest precedence; search from left to right"
  ([col]
    ;; loop through terms in the coluence
   (loop [idx 0
          col col
          lowest-idx nil
          lowest-prec @+highest-precedence+]
     ;; nothing left to process
     (if (empty? col)
       ;; return lowest found
       lowest-idx
       ;; otherwise check if current term is lower
       (let [prec (get @+precedence-table+ (first col))]
         ;; is of lower or equal precedence
         (if (and prec (<= prec lowest-prec))
           (recur (inc idx) (rest col)
                  idx prec)
           ;; is of high precedence therefore skip for now
           (recur (inc idx) (rest col)
                  lowest-idx lowest-prec)))))))

(clojure.core/defn- translate-op
  "Translation of symbol => symbol for binary op allows for
user defined operators"
  ([op]
   (get @+translation-table+ op op)))

(clojure.core/defn infix-to-prefix
  "Convert from infix notation to prefix notation"
  ([col]
   (cond
     ;; handle term only
     (not (seq? col)) col
     ;; handle sequence containing one term (i.e. handle parens)
     (= (count col) 1) (infix-to-prefix (first col))
     ;; handle all other cases
     true (let [lowest (find-lowest-precedence col)]
            (if (nil? lowest) ;; nothing to split
              col
              ;; (a b c) bind a to hd, c to tl, and b to op
              (let [[hd [op & tl]] (split-at lowest col)]
                ;; recurse

                (let [op (translate-op op)]
                  (if (seq? op)
                    (let [[method js] op]
                      (conj (list
                              (infix-to-prefix hd)
                              (infix-to-prefix tl))
                            js
                            method))
                    (list
                      (translate-op op)
                      (infix-to-prefix hd)
                      (infix-to-prefix tl))))))))))

(defmacro i
  [& code]
  (infix-to-prefix code))

;(i 1 + 1 + (3 + 6) * 8 / 5 + (fek 4 5 9) ** 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-var-args
  [args]
  (for [[arg val] (partition 2 args)]
    (cond
      (or (map? val)
          (vector? val)
          (set? val))
      `(~arg (~'transient ~val))
      (or (list? val)
          (symbol? val))
      (let [[type code] val]
        (case type
          :map `(~arg (~'transient ~code))
          :list `(~arg (~'transient ~code))
          :vector `(~arg (~'transient ~code))
          :set `(~arg (~'transient ~code))
          :bool `(~arg (~'array ~code))
          :number `(~arg (~'array ~code))))
      :else `(~arg (~'array ~val)))))

(defn- format-args-from-parse-args
  [parsed-args]
  (loop [[[x y] & code] parsed-args
         formatted []]
    (if (seq code)
      (recur code (conj formatted x y))
      (conj formatted x y))))


(defn get-types
  [args]
  (into {} (loop [[[var [val]] & rest] (partition 2 args)
                  type-list []]
             (if (seq rest)
               (recur rest (conj type-list [var val]))
               (conj type-list [var val])))))


(defn arg?
  "checks to see if x is contained in the collection
  this is used to check to see if the arguement exists in fn" ;TODO name fn
  [x coll]
  (not (empty? (filter #(= % x) coll))))

(defn arg-type
  [args x]
  (args (keyword x)))

(defn convert-arg
  [x args]
  (if (= (arg-type x args) :array)
    `(aget ~x 0)
    x))

(defn- create-get
  [x]
  (str "get-" x))
(defn- create-set
  [op a b]
  `(~'aset ~(str "set-" a) 0 (~op (~'aget ~(create-get a) 0) ~b)))


(clojure.core/defn- mark-sets
  [code]
  (clojure.walk/prewalk (fn [x] (if (list? x)
                                  (let [[a b c] x]
                                    (clojure.core.match/match [a]
                                                              ['+=!]  (create-set '+ b c)
                                                              ['-=!]  (create-set '- b c)
                                                              ['*=!]  (create-set '* b c)
                                                              ['|=!]  (create-set '/ b c)
                                                              ['set!] `(~'aset ~(str "set-" b) 0 ~c)
                                                              ['++!]  `(~'aset ~(str "set-" b) 0 (~'inc (~'aget ~(str "get-" b) 0)))
                                                              ['--!] `(~'aset ~(str "set-" b) 0 (~'dec (~'aget ~(str "get-" b) 0)))
                                                              :else x))
                                  x))
                        code))


(defn mark-gets-for-arrays
  [code types]
  (let [vars (for [[x y] types :when (= y 'array)] x)]
    (clojure.walk/postwalk
      (fn [x]
        (println x)
        (if (seq (filter #(= x %) vars))
          `(~'aget ~(str "get-" x) 0)
          x))
      code)))

(defn replace-marks-with-symbols
  [code]
  (clojure.walk/postwalk
    (fn [x]
      (if (string? x)
        (let [getter (last (re-find #"(get-)(.*)" x))
              setter (last (re-find #"(set-)(.*)" x))]
          (cond setter (symbol setter)
                getter (symbol getter)
                :else x))
        x))
    code))

(defmacro vars
  [args & code]
  (let [formatted-args (format-args-from-parse-args (parse-var-args args))
        types (get-types formatted-args)]
    `(let ~formatted-args
       ~@(replace-marks-with-symbols
           (mark-gets-for-arrays (mark-sets code) types)))))

;(vars [x 1] (++! x) x)


