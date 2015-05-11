(ns snake-web.entity-gameworld
  (:require snake-web.dom-wrapper))

(def game-world
  (let [world-pixel-size 500]
    {
     :delta-accum 0
     :global-timer 0
     :move-allowed false
     :width world-pixel-size
     :height world-pixel-size
     :cell-size 25
     :cell-padding 0

     :cells (into {}
                  (map vec
                       (partition 2 (interleave (for [x (range 1 (inc (* 20 20)))] ((fn [x] x)
                                                                                     (keyword (str x))))

                                                (for [x (range 0 20) y (range 0 20)] [(* 25 y) (* 25 x)]))
                                  )))

     ;;GRAPHICS
     :cell-padding-color (snake-web.dom-wrapper/rgba 0 0 0 1) ;black
     :gradient-position [500 500]
     :gradient-point [0 0]

     :update (fn [this entities]
               (let [delta-accum (+ (:delta-accum this) (snake-web.game-engine/delta))

                     move-allowed (if (>= delta-accum 1.0)
                                    true
                                    false
                                    ;(inc (:global-timer this))
                                    ;(:global-timer this)
                                    )]
                 (conj this
                   (conj {:delta-accum (if (>= delta-accum 1.0) 0.0 delta-accum)
                          ;:global-timer global-timer
                          :move-allowed move-allowed}))))
   }))



(defn debug-game-world
  []
  (let [coords (for [x (range 0 20) y (range 0 20)] [(* 25 y) (* 25 x)])
        offset 0]
    (.clearRect
      (snake-web.development/game-window :context)
      0 0
      (.-width (snake-web.development/game-window :canvas))
      (.-height (snake-web.development/game-window :canvas)))
    (doseq [[x y] coords]
      (snake-web.dom-wrapper/draw-rect! {:x (- x offset)
                                         :y (- y offset)
                                         :width 25
                                         :height 25
                                         :fill-style (snake-web.dom-wrapper/rgba (rand-int (inc 255))
                                                                                 (rand-int (inc 255))
                                                                                 (rand-int (inc 255))
                                                                                 1)
                                         :context (snake-web.development/game-window :context)}))))