(ns snake-web.dom-wrapper-macros)

(def context-setters {:stroke-style '.-strokeStyle
                      :fill-style '.-fillStyle
                      :line-width '.-lineWidth})

(defn my-test-macro
  [key value context]
  `(
     ~(key context-setters)
     ~context
     ~value
     )
  )