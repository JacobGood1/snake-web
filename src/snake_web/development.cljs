(ns snake-web.development
  (:require snake-web.dom-wrapper
            snake-web.my-cljs-tools
            snake-web.game-engine
            snake-web.input
            [snake-web.entities.gameworld :as game-world]
            [snake-web.entities.snake :as snake]
            [snake-web.entities.apple :as apple]))

(defonce game-window (let [[canvas context]
                           (snake-web.dom-wrapper/create-canvas 500 500)]
                       (.appendChild (.-body js/document) canvas)

                       ;temporary VVV
                       (-> canvas
                           (.-style)
                           (.-border)
                           (set! "1px solid black"))

                       {:canvas canvas :context context}))

(snake-web.input/setup-input-handler)

(defn update-entities
  [key value entities]

  (if (vector? value)

    ;(assoc entities :game-world )
    {key (vec (for [obj value]
                ((:update obj) obj entities)))}

    (if (contains? value :update)
      ;(update-in obj [key] conj
      {key ((:update value) value entities)}
      ;)
      ;update not found, return original map
      {key value})))

(def entities
  (atom
    {:game-world game-world/game-world
     :snakes [snake/snake]
     :apple apple/apple}))

(defn clear-screen
  []
  (.clearRect
    (game-window :context)
    0 0
    (.-width (game-window :canvas))
    (.-height (game-window :canvas))))

(defn user-game-loop
  []
  ;(snake-web.entities.gameworld/timer)
  ;(println (snake-web.game-engine/delta))
  ;(println (snake-web.game-engine/fps))

  ;;clear canves
  (clear-screen)

  ;;traverse through every map and invoke all update fns.
  (swap! entities (fn [obj]
                    (conj obj (reduce conj (map conj
                                                (for [[key value] obj]
                                                  (update-entities key value obj)))))))

  ;;traverse through every map and invoke all render fns.
  (doseq [[key value] @entities]
    (if (vector? value)

      (doseq [obj value]
        (if (contains? obj :render) ((:render obj) obj)))

      (if (contains? value :render) ((:render value) value))))

  ;quick hack for rendering GAME OVER text ontop of every element in the scene
  ;this should exist within the gameworld's update fn
  (if (not (:alive? (first (:snakes @entities))))
    (do
      (set! (.-font (:context snake-web.development/game-window)) "50px Arial")
      (set! (.-fillStyle (:context snake-web.development/game-window)) "blue")
      (.fillText (:context snake-web.development/game-window)
                 "GAME OVER"
                 (- (/ 500 2) 150)
                 (/ 500 2))))

  ;(println @entities)
  ;(println (:delta-accum (:game-world @entities)))
  ;(println (:global-timer (:game-world @entities)))
  ;(println (:body-segments (first (:snakes @entities))))
)

(defn game-loop
  "WARNING: game logic does NOT belong here."
  []
  (.requestAnimationFrame js/window
                          (fn []
                            (game-loop)
                            ))
  (user-game-loop))

(defn main [] (game-loop))