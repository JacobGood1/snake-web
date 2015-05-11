(ns snake-web.input
  (:require [snake-web.my-cljs-tools :as tools]))

;;TODO
;;As of right now, we only have a single map containing
;;input keys and their on/off states.
;;Need to implement 3 separate maps for the different
;;types of inputs. (ex. Pressed, Hold, and Released)

;;TODO
;;convert input-key-values and it's brother to macros.

;;;; map containing input keys with their key-code
;;;; order: {:a 1}
(def input-key-values
  (-> (->> (interleave (map keyword
                            "abcdefghijklmnopqrstuvwqyz")
                       (range 65 91))
           (partition 2)
           (map vec)
           (into {}))
      (conj {:space 32}
            {:enter 13}
            {:lastkeypressed :x})))

;;;; map containing input keys with their key-code
;;;; order: {1 :a}
(def input-value-keys
  (into {} (map (comp vec reverse) input-key-values)))

;;;; converted to JS
;;;; map containing input keys with their on/off value
;;;; order: {:a false}
(def *input-keys*
  (clj->js (into {}
                 (for [[key _] input-key-values]
                   [key false]))))

;;BOOKMARK

(defn set-key-state!
  [event bool]
  (aset *input-keys* (-> (.-keyCode event)
                         (input-value-keys)
                         (tools/key->str))
        bool)
  (aset *input-keys* "lastkeypressed"
        (-> (.-keyCode event)
            (input-value-keys))))

;;check for all key events and mutate the input-keys object
(defn key-down-handler!
  [event]
  (set-key-state! event true)
  ;;BOOKMARK println for debugging
  ;(println "key-down: " (.-keyCode event))
  ;(println (.-lastkeypressed *input-keys*))
  )

(defn key-up-handler!
  [event]
  (set-key-state! event false)
  ;;BOOKMARK println for debugging
  ;(println "key-up: " (.-keyCode event))
  )


(defn setup-input-handler []
  ;keydown
  (set! (.-onkeydown js/window) key-down-handler!)
  ;keyup
  (set! (.-onkeyup js/window) key-up-handler!))