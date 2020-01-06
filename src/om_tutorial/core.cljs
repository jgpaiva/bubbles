(ns om-tutorial.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(def app-state (atom {:count 1}))

(defn make-circle [x y r] (dom/circle #js {:key (hash (str x y r)) :cx x :cy y :r r :fill "#ff0066"}))

(defn test-eq [name a b] (assert (= a b) (str name "FAIL: " a " is not equal to " b)))

(defn distance[a b]
  (let [p1 (- (:x b) (:x a))
        p2 (- (:y b) (:y a))]
    (Math.sqrt (+ (* p1 p1) (* p2 p2)))))
(test-eq "calculates distance between two points"
        (distance {:x 1 :y 1} {:x 1 :y 10}),
        9)
(test-eq "also works reversed"
        (distance {:x 10 :y 1} {:x 1 :y 1}),
        9)
(test-eq "works in other cases"
        (distance {:x 1 :y 1} {:x 2 :y 2}),
        (Math.sqrt 2))


(defui Counter
  Object
  (render [this]
          (let [{:keys [count]} (om/props this)]
            (dom/div nil
                     (dom/span nil (str "Count: " count))
                     (dom/button
                      #js {:onClick
                           (fn [e]
                             (swap! app-state update-in [:count] inc))}
                      "Click me!")
                     (dom/svg nil
                              (map (fn [x] [(make-circle (+ 20 (* 20 x)) 20 10)]) (range count)))
                     ))))

(def reconciler
  (om/reconciler {:state app-state}))

(om/add-root! reconciler
              Counter (gdom/getElement "app"))
