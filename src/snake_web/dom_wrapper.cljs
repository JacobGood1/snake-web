(ns snake-web.dom-wrapper
  (:use-macros [snake-web.dom-wrapper-macros :only [my-test-macro]])
  (:require snake-web.my-cljs-tools
            [cljs.core.match :refer-macros [match]]))

(def html-elements {:body "body"
                    :p "p"
                    :canvas "canvas"})

(def color {:black "black"})

(defn rgba
  "Returns rgba in a string format."
  [r g b a]
  (str "rgba(" r ", " g ", " b ", " a ")"))

(defn group-points
  [points]
  (let [last-p (last points)
        first-p (first points)
        last-group [last-p first-p]]
    (loop [[p1 p2 :as points] points
           grouped-coordinates []]
      (match [points]
             [[]] nil
             [[_]] (conj grouped-coordinates last-group)
             :else (recur (vec (rest points))
                          (conj grouped-coordinates [p1 p2]))))))



(defn draw-line!
  [start end line-width color context]
  (let [[sx sy] start [ex ey] end]
    (set! (.-strokeStyle context) (color color))
    (set! (.-lineWidth context) line-width)
    (.beginPath context)
    (.moveTo context sx sy)
    (.lineTo context ex ey)
    (.stroke context)))

(defn draw-polygon!
  [points line-width color context]
  (doseq [[p1 p2] (group-points points)] (draw-line! p1 p2 line-width color context)))

(defn draw-rect!
  [& args]
  (let [{:keys [x y width height context fill-style stroke-style line-width]} (apply hash-map args)]

    ;variable setters
    (if (not (nil? stroke-style)) (set! (.-strokeStyle context) stroke-style))
    (if (not (nil? line-width)) (set! (.-lineWidth context) line-width))
    (if (not (nil? fill-style)) (set! (.-fillStyle context) fill-style))

    ;this will always be called
    (.beginPath context)
    (.rect context x y width height)

    ;variable methods
    (if (not (nil? stroke-style)) (.stroke context))
    (if (not (nil? fill-style)) (.fill context))))



(defn create-canvas
  "create a canvas and return itself as well as it's context"
  [width height]
  (let [canvas (.createElement js/document "canvas")
        context (.getContext canvas "2d")]
    (.setAttribute canvas "width" (str width))
    (.setAttribute canvas "height" (str height))
    (.setAttribute canvas "id" "canvas")
    [canvas context]))

(defn append-child!
  [element object]
  (.appendChild (.getElementById js/document (html-elements element)) object))