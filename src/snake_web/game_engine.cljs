(ns snake-web.game-engine)

(def fps
  (let [last-called-time (clj->js [0])
        fps (clj->js [0])
        delta (clj->js [0])

        s! (fn [x v] (aset x 0 v))]
    ;(s! fps 276)

    (fn []
      (if (= 0 last-called-time)
        (do
          (s! last-called-time (.now js/Date))
          (s! fps 0))
        (do
          (s! delta (/ (- (.getTime (new js/Date)) last-called-time) 1000))
          (s! last-called-time (.now js/Date))
          (s! fps (/ 1 delta)))
        )

      (aget fps 0)
      )))

(def delta
  (let [last-called-time (clj->js [0.0])
        delta (clj->js [0.0])
        s! (fn [x v] (aset x 0.0 v))]
    (fn []
      (if (= 0.0 last-called-time)
        (do
          (s! last-called-time (.now js/Date))
          )
        (do
          (s! delta (/ (- (.getTime (new js/Date)) last-called-time) 1000))
          (s! last-called-time (.now js/Date))
          )
        )
      (aget delta 0)
      )))

