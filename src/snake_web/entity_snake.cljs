(ns snake-web.entity-snake
  (:require snake-web.dom-wrapper
            [snake-web.input :as input]))

(def body-segment
  {
   ;size and starting position are tied to the size of the map
   :pos [100 100]
   :size 25
   :step-size 25
   :direction :x

   ;;GRAPHICS
   ;color
   :fill-style (snake-web.dom-wrapper/rgba 0 255 0 1.0)
   ;stroke line width
   ;:line-width 3
   ;stroke color
   ;:stroke-style (snake-web.dom-wrapper/rgba 0 0 0 1.0)
   }
  )

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
  (if (= (:move-allowed (:game-world entities)) true)
    (let [head (snake-head-update this)
          ;update snake's tail, based off it's un-atlered head position
          tail (snake-tail-update (:body-segments this))]

      (assoc this :body-segments (conj (seq tail) head)))

    ;snake is not allowed to move, return un-altered snake.
    this))

(defn append-segment
  [this pos]
  ;{pre: [(vector? pos)]}
  (let [body-segments (:body-segments this)
        new-segment (assoc body-segment :pos pos)
        updated-body-segments (conj body-segments new-segment)]
    (assoc this :body-segments updated-body-segments)))

(defn snake-update
  "Update the snake object."
  [this entities]
  ;firstly, update the body of the snake
  (let [last-segment-pos (get-last-segement-pos this)
        snake (snake-body-update this entities)]
    ;check if an apple has been collected, if so, append a new segment
    (if (:apple-collected snake)
      (append-segment snake last-segment-pos)
      ;no apple was collected, return snake
      snake))
  )

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
        :context (snake-web.development/game-window :context)
        ))))

(def snake
  {:body-segments [body-segment]
   :apple-collected true ;default: false
   :update snake-update
   :render snake-render})