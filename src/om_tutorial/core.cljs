(ns om-tutorial.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [cljs.test :refer-macros [deftest is testing run-tests]]
            [clojure.datafy :as d]))

(def app-state (atom {:count 1}))

(defn draw-circle [circle]
  (dom/circle #js {
                   :key (hash (str circle))
                   :cx (:x (:p circle))
                   :cy (:y (:p circle))
                   :r (:r circle)
                   :fill "#ff0066"
                   }))


(defn distance[a b]
  (let [p1 (- (:x b) (:x a))
        p2 (- (:y b) (:y a))]
    (Math.sqrt (+ (* p1 p1) (* p2 p2)))))
(deftest test-distance
  (testing "calculates distance between two points"
    (is (= (distance {:x 1 :y 1} {:x 1 :y 10}) 9)))
  (testing "also works reversed"
    (is (= (distance {:x 10 :y 1} {:x 1 :y 1}) 9)))
  (testing "works in other cases"
    (is (= (distance {:x 1 :y 1} {:x 2 :y 2}) (Math.sqrt 2)))))

(defn gen-circle
  ([minRadius maxRadius width height circles]
   (gen-circle minRadius maxRadius width height circles Math.random))
  ([minRadius maxRadius width height circles random]
   (let [r (+ minRadius (Math.round (* (random) (- maxRadius minRadius))))
         p {
            :x (Math.round (+ r (* (random) (- width (* 2 r)))))
            :y (Math.round (+ r (* (random) (- height (* 2 r)))))
            }
         m (apply min (map (fn [x]
                             (let [d (distance p (:p x))]
                               (Math.floor (- d (:r x)))))
                           circles))
         ]
     (if (or (not m) (>= m minRadius))
       {:p p, :r (min m r)}))))
(deftest test-gen-circle
  (testing "creates circles"
    (is (= (gen-circle 1 10 100 100 []  (fn [] 0.5)) {:p {:x 50 :y 50} :r 6})))
  (testing "returns the minimum radius if the circle would overlap with an existing one"
    (is (= (gen-circle 1 10 100 100 [{:p {:x 50 :y 55} :r 2}]  (fn [] 0.5))
           {:p {:x 50 :y 50} :r 3})))
  (testing "returns empty when it's impossible to generate without overlap"
    (is (= (gen-circle 1 10 100 100 [{:p {:x 50 :y 55} :r 5}]  (fn [] 0.5)) nil))))

(defn gen-circles [sizeDiff zoom targetOccupation]
  (let [minRadius 10
        width (* zoom minRadius)
        ratio 2.4
        height (Math.round (/ width ratio))
        area (* width height)
        maxRadius (min (/ width 2) (/ height 2) (* minRadius sizeDiff))
        iterations (/ (* width height) (* minRadius minRadius))]
    (:circles
     (reduce
      (fn [{:keys [circles occupation]}]
        (if (< (/ occupation area) targetOccupation)
          (if-let [circle (gen-circle minRadius maxRadius width height circles)]
            {:circles (conj circles circle)
             :occupation (+ occupation
                            (* Math.PI (:r circle) (:r circle)))}
            {:circles circles :occupation occupation})
          {:circles circles :occupation occupation}))
      {:circles [] :occupation 0}
      (range iterations)))))
(deftest test-gen-circles
  (testing "tries to occupy to the target occupation"
    (is (= (gen-circles 10 1 -1) [])))
  (testing "may go over the target occupation, but it stops there"
    (is (= (count (gen-circles 10 10 0.001)) 1)))
  (testing "creates lots of circles if there's space and the target occupation so requires"
    (is (> (count (gen-circles 1 100 0.5)) 10)))
  (testing "will not create lots of circles if the target occupation is low"
    (is (= (count (gen-circles 1 100 0.00000001)) 1))))

(def width 800)
(def height 600)

(defui Counter
  Object
  (render [this]
          (let [{:keys [count]} (om/props this)]
            (dom/div nil
                     (dom/span nil (str "Count: " count))
                     (dom/br nil)
                     (dom/input #js {:type "range" :min 1 :max 100 :value count
                                     :onChange (fn [e] (swap! app-state update-in [:count] (fn [_] (int (-> e .-target .-value)))))})
                     (dom/br nil)
                     (dom/button
                      #js {:onClick (fn [e] (swap! app-state update-in [:count] inc))}
                      "Click me!")
                     (dom/br nil)
                     (dom/svg #js {:width width :height height}
                              (map draw-circle
                                   (reduce
                                    (fn [circles _]
                                      (if-let [circle (gen-circle 1 30 width height circles)]
                                        (conj circles circle)
                                        circles))
                                    []
                                    (range count))))))))

(def reconciler
  (om/reconciler {:state app-state}))

(om/add-root! reconciler
              Counter (gdom/getElement "app"))

(cljs.test/run-tests)
