(ns ^:figwheel-always snake-web.core
  (:require ;snake-web.dom-wrapper
            ;snake-web.my-cljs-tools
            ;snake-web.game-engine
            ;snake-web.input
            snake-web.development
    [cljs.core.match :refer-macros [match]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer con.")

;; define your app data so that it doesn't get over-written on reload

;(defonce app-state (atom {:text "Hello world!"}))

(defonce main {:main (snake-web.development/main)})

;(.appendChild (.-body js/document)


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

; Start REPL
;(use 'figwheel-sidecar.repl-api)
;(cljs-repl)