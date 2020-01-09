(ns bubbles.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(enable-console-print!)

(defonce app-state (atom {
                          :sizeDiff 1
                          :zoom 1.0
                          :targetOccupation 0.25
                          :hpoint 300
                          :hrange 0
                          :spoint 99 ; tricky: can't use 100, because it wraps to 0
                          :srange 0
                          :lpoint 50
                          :lrange 0
                          :selected false
                          }))

(def width 800)
(def height 600)

(defn encode [params]
  (->> params
       (sort-by first)
       (map (fn [[param {:keys [min max value]}]]
              (Math.round (* 256 (/ (- value min) (- max min))))))
       (reduce (fn [acc x] (bit-or (bit-shift-left acc 8) x)) 0)))
(deftest test-encode
  (testing "it encodes params in bytes"
    (is (= (encode {:param1 {:min 0 :max 100 :value 50}}) 0x80)))
  (testing "it encodes several params in bytes"
    (is (= (encode {:param1 {:min 1 :max 11 :value 6}
                    :param2 {:min 0 :max 100 :value 25}}) 0x8040)))
  (testing "it encodes always in the same order"
    (is (= (encode {:param1 {:min 0 :max 100 :value 50}
                    :param2 {:min 0 :max 100 :value 25}})
           (encode {:param2 {:min 0 :max 100 :value 25}
                    :param1 {:min 0 :max 100 :value 50}})))))

(defn decode [bits params]
  (let [sorted-params (sort-by first params)
        partitioned-bits (->> params
                              (reduce
                               (fn [{:keys [bits retval]}, _]
                                 {:bits (bit-shift-right bits 8)
                                  :retval (conj retval (bit-and bits 0xff))})
                               {:bits bits :retval []})
                              :retval
                              reverse)]
    (into {} (map (fn [[k v] bits]
                    (let [min (:min v)
                          max (:max v)]
                      [k (assoc v :value (Math.round (+ min (* (- max min) (/ bits 0xff)))))]))
                  sorted-params
                  partitioned-bits))))
(deftest test-decode
  (testing "it decodes bytes into params"
    (is (= (decode 0x80 {:param1 {:min 0 :max 100}}) {:param1 {:min 0 :max 100 :value 50}})))
  (testing "it works with several several params"
    (is (= (decode 0x8040 {:param1 {:min 1 :max 11} :param2 {:min 0 :max 100}})
           {:param1 {:min 1 :max 11 :value 6} :param2 {:min 0 :max 100 :value 25}})))
  (testing "it decodes always in the same order"
    (is (= (decode 0xf022 {:param1 {:min 0 :max 100} :param2 {:min 0 :max 100}})
           (decode 0xf022 {:param2 {:min 0 :max 100} :param1 {:min 0 :max 100}})))))

(defn gen-individual
  ([params] (gen-individual params Math.random))
  ([params random] (->> params
                        (map (fn [[k v]] [k (assoc v :value (+ (:min v) (* (random) (- (:max v) (:min v)))))]))
                        (into {}))))
(deftest test-gen-individual
  (testing "it generates random individuals given params"
    (is (every? (fn [[k v]] (:value v)) (gen-individual {:param1 {:min 1 :max 11} :param2 {:min 0 :max 100}})))))

(defn float= [a b]
  (< (Math.abs (- a b)) 0.0000001))

(defn pick-from-range
  ([min max point range] (pick-from-range min max point range Math.random))
  ([min max point range random]
   (+ min (mod (+ (- point (/ range 2)) (* (random) range)) (- max min)))))
(deftest test-pick-from-range
  (testing "when range is small, always chooses point"
    (is (float= (pick-from-range 0 10 1 0) 1)))
  (testing "supports wrap around and max is exclusive"
    (is (float= (pick-from-range 0 10 10 2 (fn [] 1)) 1)))
  (testing "the wrap-around is tricky, max can never be selected"
    (is (float= (pick-from-range 0 10 10 0 (fn [] 1)) 0)))
  (testing "works with floats"
    (is (float= (pick-from-range 0 10 10 1 (fn [] 1)) 0.5)))
  (testing "works with floats"
    (is (float= (pick-from-range 0 100 43 2 (fn [] 0.2)) 42.4))))

(defn pick-color
  ([hpoint hrange spoint srange lpoint lrange]
   (pick-color hpoint hrange spoint srange lpoint lrange Math.random))
  ([hpoint hrange spoint srange lpoint lrange random]
   (str
    "hsl("
    (Math.round (pick-from-range 0 360 hpoint hrange random))
    ", "
    (Math.round (pick-from-range 0 100 spoint srange random))
    "%, "
    (Math.round (pick-from-range 0 100 lpoint lrange random))
    "%)")))
(deftest test-pick-color
  (testing "selects a random valid color"
    (is (= (pick-color 0 0 0 0 0 0) "hsl(0, 0%, 0%)")))
  (testing "knows how to wrap around"
    (is (= (pick-color 360 2 100 2 100 2 (fn [] 1)) "hsl(1, 1%, 1%)")))
  (testing "always rounds"
    (is (= (pick-color 50 1 43 2 29 3 (fn [] 0.2)) "hsl(50, 42%, 28%)")))
  (testing "picks the right colors"
    (is (= (pick-color 300 0 99 0 50 0) "hsl(300, 99%, 50%)"))))

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
  (let [minRadius (* zoom 10)
        area (* width height)
        maxRadius (min (/ width 2) (/ height 2) (* minRadius sizeDiff))
        iterations (* 10 (/ (* width height) (* Math.PI minRadius minRadius)))]
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
    (is (> (count (gen-circles 1 10 0.5)) 0.9)))
  (testing "will not create lots of circles if the target occupation is low"
    (is (= (count (gen-circles 1 10 0.00000001)) 1))))

(defn color-circle [hpoint hrange spoint srange lpoint lrange circle]
  (assoc circle :f (pick-color hpoint hrange spoint srange lpoint lrange)))

(defn draw-circle [circle]
  [:circle {
            :key (hash (str circle))
            :cx (:x (:p circle))
            :cy (:y (:p circle))
            :r (:r circle)
            :fill (:f circle)
            }])

(defn draw-range [param min max step converter-function]
  [:div {:class "range-container"}
   [:input {:type "range" :min min :max max :step step :value (param @app-state) :key (hash (str "range " param)) :class "slider"
               :onChange (fn [e] (swap! app-state update-in [param] (fn [_] (converter-function (-> e .-target .-value)))))}]
   [:span nil (str param ":" (param @app-state))]])

(defn main-render []
  [:div
   [:div {:class "settings-container"}
    (draw-range :sizeDiff 1 30 1 int)
    (draw-range :zoom 1 7 0.05 float)
    (draw-range :targetOccupation 0.01 0.40 0.01 float)
    (draw-range :hpoint 0 360 1 int)
    (draw-range :hrange 0 360 1 int)
    (draw-range :spoint 0 99 1 int)
    (draw-range :srange 0 99 1 int)
    (draw-range :lpoint 0 99 1 int)
    (draw-range :lrange 0 99 1 int)
    ]
   [:div {:class "svg-container-selected"}
    [:svg {:width width :height height}
     (->> (gen-circles (:sizeDiff @app-state) (:zoom @app-state) (:targetOccupation @app-state))
          (map (partial color-circle (:hpoint @app-state) (:hrange @app-state) (:spoint @app-state) (:srange @app-state) (:lpoint @app-state) (:lrange @app-state)))
          (map draw-circle))]]
   ])

(reagent/render-component [main-render]
                          (. js/document (getElementById "app")))

(cljs.test/run-tests)
