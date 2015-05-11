(ns snake-web.input)

(def input-key-values
  (-> (->> (interleave (map (comp keyword str)
                            "abcdefghijklmnopqrstuvwqyz")
                       (range 65 91))
           (partition 2)
           (map vec)
           (into {}))
      (conj {:space 32})))

(def input-value-keys
  (into {} (map (comp vec reverse) input-key-values)))

(defn determine-keyword
  [x]
  (println x)
  ;(input-value-keys 83)
  (input-value-keys x)
  ) ;(.-keyCode ~(symbol "event"))

(defn keyword->methodcall
  [key]
  (symbol (apply str (conj (rest (str key)) ".-"))))

(defmacro key-down-handler-macro!
  [input-keys key-code]
  `(set! ((keyword->methodcall (input-value-keys ~key-code)) ~input-keys) true))

(defmacro key-up-handler-macro!
  [input-keys key-code]
  `(set! ((keyword->methodcall (input-value-keys ~key-code)) ~input-keys) false))

(key-down-handler-macro! *input* (.-keyCode event))