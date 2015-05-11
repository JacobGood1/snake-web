(ns snake-web.entities.apple
  (:require
    [snake-web.entities.gameworld :as game-world]
    [snake-web.dom-wrapper :as dom-wrapper]))

(defn apple-update
  [this entities]
  this)

(defn apple-render
  [this]
  (let [[x y] (:pos this)
        size (:size this)
        color (:fill-style this)]
    (snake-web.dom-wrapper/draw-rect!
      :x x
      :y y
      :width size
      :height size
      :fill-style color
      :context (snake-web.development/game-window :context)
      )))

(def apple
  {:pos [300 300]
   :size 25
   :fill-style (dom-wrapper/rgba 255 0 0 1.0)
   :render apple-render})

(defn filter-available-cells
  [entities]
  (filter (fn [x]
            (not= nil x)) (for [[num pos] (:cells game-world/game-world)
                                body-segments (:body-segments(first (:snakes entities)))]
                            (if (not= (:pos body-segments) pos)
                              pos)))
  )

(defn find-spawn-location
  "Find a proper location to spawn an apple.
  An apple can only spawn in a cell which is not already occupied."
  [entities]
  (let [cells (:cells (:game-world entities))
        ;TODO adjust for multiplayer
        ;multiplayer has been scrapped for the time being,
        ;grab the one and only snake from the 'snakes' vector
        snake (first (:snakes entities))
        body-segments (:body-segments snake)
        available-cells (filter-available-cells entities)
        random-cell (nth available-cells (rand-int (count available-cells)))]
    random-cell))

(defn spawn-apple
  [entities]
  (let [apple (assoc apple :pos (find-spawn-location entities))]
    apple))