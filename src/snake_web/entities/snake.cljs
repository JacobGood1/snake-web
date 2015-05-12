(ns snake-web.entities.snake
  (:require snake-web.dom-wrapper
            snake-web.entities.gameworld
            [snake-web.input :as input]))

(def body-segment
  {
   ;size and starting position are tied to the size of the map
   :pos [100 100]
   :size 25
   :step-size 25

   ;;GRAPHICS
   :fill-style (snake-web.dom-wrapper/rgba 0 255 0 1.0)
   ;stroke line width
   ;:line-width 3
   ;stroke color
   ;:stroke-style (snake-web.dom-wrapper/rgba 0 0 0 1.0)
   }
  )

(defn get-snake-head
  [this]
  (-> this
      (:body-segments)
      (first)))

(defn apple-collision?
  [this entities]
  ;(println (:apple entities))
  (let [apple (:apple entities)
        snake-head (get-snake-head this)]
        (= (:pos apple) (:pos snake-head))))

(defn passed-bounds?
  [this entities]
  (let [height (:height (:game-world entities))
        width (:width (:game-world entities))
        {[x y] :pos size :size} (get-snake-head this)]
    (cond (or (> (+ x size) width)
              (< x 0)) true
          (or (> y (- height size))
              (< y 0)) true
          :else false)))

(defn collision-with-self?
  [{[head & body-segments] :body-segments}]
  (loop [body-segments body-segments]
    (if (seq body-segments)
      (if (= (:pos head) (:pos (first body-segments)))
        true
        (recur (rest body-segments)))
      false)))

(defn snake-collision-death?
  [this entities]
  (assoc this :alive? (not (or (passed-bounds? this entities)
                               (collision-with-self? this)))))

(defn get-last-segement-pos
  [this]
  (let [last-body-segment (last (:body-segments this))]
    (:pos last-body-segment)))

(defn travel-direction
  []
  ;TODO find out why this println is called twice?
  ;(println "i get called twice for some reason")
  (let [key (aget input/*input-keys* "lastkeypressed")]
    (cond (= key :a) :-x
          (= key :d) :x
          (= key :w) :-y
          (= key :s) :y
          :else :x)))

(defn snake-head-update
  [this]
  (let [head (first (:body-segments this))
        [x y] (:pos head)
        direction (travel-direction)
        size (:size head)]
    ;(println direction)
    (cond (= :x direction) (assoc head :pos [(+ x size) y])
          (= :-x direction) (assoc head :pos [(- x size) y])
          (= :y direction) (assoc head :pos [x (+ y size)])
          (= :-y direction) (assoc head :pos [x (- y size)]))))

(defn snake-tail-update
  [segments]
  (loop [prv-val (first segments)
         li (rest segments)
         new-li []]
    (let [first-li (first li)
          rest-li (rest li)]
      (cond (empty? li) new-li
            :else (recur first-li
                         rest-li
                         (conj new-li
                               (assoc first-li :pos (:pos prv-val))))))))

(defn snake-body-update
  "Update the entire body of the snake."
  [this entities]
  ;update snake's head
    (let [head (snake-head-update this)
          ;update snake's tail, based off it's un-atlered head position
          tail (snake-tail-update (:body-segments this))]
      (assoc this :body-segments (conj (seq tail) head)))
    ;snake is not allowed to move, return un-altered snake.
)

(defn append-segment
  [this pos]
  ;{pre: [(vector? pos)]}
  (let [body-segments (:body-segments this)
        color (- (:color this) 5)
        new-segment (assoc body-segment :pos pos :fill-style (snake-web.dom-wrapper/rgba 0 color 0 1.0))
        updated-body-segments (conj (vec body-segments) new-segment)
        snake (assoc this :body-segments updated-body-segments :color color)]
    snake))

(defn snake-movement-update
  [this entities]
  (if (= (:move-allowed (:game-world entities)) true)
    ;update the position of the snake
    (snake-body-update this entities)
    ;movement not allowed, return snake
    this))

(defn snake-apple-update
  [this entities last-segement-pos]
  ;check if an apple has been collected, if so, append a new segment
  ;(println (apple-collision? this entities))

  ;TODO study
  ;without this if statement, both the apple's position and snake's appendage update would
  ;happen twice
  (if (not (:apple-collected this)) ;<-- fixes double update glitch
    (if (apple-collision? this entities)
      (do
        (snake-web.entities.gameworld/timer)
        (assoc (append-segment this last-segement-pos)
          :apple-collected true
          :travel-speed (- (:travel-speed this) (* (:travel-speed-dec this) (:travel-speed this)))))
      ;no apple was collected, return snake
      (assoc this :apple-collected false))
    (assoc this :apple-collected false)))

(defn snake-update
  "Update the snake object."
  [this entities]
  ;(println (apple-collision? this entities))
  ;(println (passed-bounds? this entities))
  (if (:alive? this)
    (let [last-segment-pos (get-last-segement-pos this)
          snake (snake-movement-update this entities)
          snake (snake-apple-update snake entities last-segment-pos)
          snake (snake-collision-death? snake entities)]
      snake)
    this))

(defn snake-render
  [this]
    (doseq [segment (:body-segments this)]
      (let [[x y] (:pos segment)
            size (:size segment)
            color (:fill-style segment)]
      (snake-web.dom-wrapper/draw-rect!
        :x x
        :y y
        :width size
        :height size
        :fill-style color
        :context (snake-web.development/game-window :context)))))

(def snake
  {:body-segments [body-segment
                   ]
   :alive? true
   :travel-speed 0.5
   :travel-speed-dec 0.05
   :color 255
   :apple-collected false
   :update snake-update
   :render snake-render})