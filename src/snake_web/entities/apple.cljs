(ns snake-web.entities.apple
  (:require
    [snake-web.entities.gameworld :as game-world]
    [snake-web.dom-wrapper :as dom-wrapper]))

(defn apple-update
  [this entities]
  (let [snakes (:snakes entities)]
    ;(print (:apple-collected (first (:snakes entities))))
    (if (first (filter (fn [snake] (:apple-collected snake)) snakes))
      (snake-web.entities.apple/spawn-apple entities)
      this)))

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
  {:pos [200 100]
   :size 25
   :fill-style (dom-wrapper/rgba 255 0 0 1.0)
   :render apple-render
   :update apple-update})

(defn filter-available-cells*
  [entities]
  (filter (fn [x]
            (not= nil x)) (for [[num pos] (:cells game-world/game-world)
                                body-segments (:body-segments(first (:snakes entities)))]
                            (if (not= (:pos body-segments) pos)
                              pos)))
  )



(defn filter-available-cells**
  [entities]
  ;(println "Aasdasdasd")
  (let [{[{body-segments :body-segments}] :snakes} entities
        {{cells :cells} :game-world} entities
        cells (snake-web.entities.gameworld/get-cells cells)]

    (let [kek (loop [[{body-segment-pos :pos} :as body-segments] body-segments
                     available-cells cells]

                (if (seq body-segments)
                  (recur (rest body-segments)
                         (reduce conj available-cells
                                 (loop [[cell :as cells] available-cells
                                         available-cells []]
                                   (if (seq cells)
                                     (if (not= cell body-segment-pos)
                                       (recur (rest cells) (conj available-cells cell))
                                       (recur (rest cells) available-cells))
                                     available-cells))))
                  available-cells))]
      ;(println (count body-segments))
      ;(println (count kek))
      kek)))

(defn in?
  [val coll]
  (not (empty? (filter (fn [x]
                         (= x val))
                       coll))))

(defn filter-available-cells
  [{[{body-segments :body-segments}] :snakes {cells :cells} :game-world}]
  (let [cells (snake-web.entities.gameworld/get-cells cells)]
    (loop [s1 (map :pos body-segments)
           [s2 :as s2s] cells
           new-list []]
      (if (seq s2)
        (recur s1 (rest s2s) (if-not (in? s2 s1)
                               (if-not (in? s2 new-list)
                                 (conj new-list s2)
                                 new-list)
                               new-list))
        new-list))))

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
        random-cell (rand-nth available-cells)]
    ;(println (count available-cells))
    ;(println (count cells))
    ;(println (count available-cells))

    random-cell))

(defn spawn-apple
  [entities]
  (let [apple (assoc snake-web.entities.apple/apple :pos (find-spawn-location entities))]
    apple))