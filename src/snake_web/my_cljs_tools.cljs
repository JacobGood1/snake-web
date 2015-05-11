(ns snake-web.my-cljs-tools)

(defn key->str
  [key]
  (->> (-> key
           str
           rest)
       (apply str)))

(defn vector-to-map
  "Takes a vector of key-value pairs and returns a map"
  [args]
  (loop [[key value :as args] args
         grouped {}]
    (cond
      (empty? args) grouped
      :else (recur (rest (rest args)) (conj grouped {key value})))))