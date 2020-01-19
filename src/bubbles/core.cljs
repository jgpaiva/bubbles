(ns bubbles.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is testing run-tests]]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as stc]
            [clojure.spec.test.alpha :as stest]))

(enable-console-print!)

(def width 400)
(def height 400)

(defn encoded-op [op fst snd]
  (let [[fst snd] (if (>= (count fst) (count snd)) [fst snd] [snd fst])]
    (clojure.string/join
     (reverse (map (fn [a b] (str (op (int a) (int b))))
                   (reverse fst)
                   (concat (reverse snd) (repeat "0")))))))
(deftest test-encoded-op
  (testing "it applies a bitwise operation to two encoded objects"
    (is (= (encoded-op bit-or "00000001" "00000010") "00000011"))
    (is (= (encoded-op bit-and "00001001" "00001010") "00001000")))
  (testing "it always left-pads with zeroes to apply the operation"
    (is (= (encoded-op bit-or "0000000000000001" "00000010") "0000000000000011"))
    (is (= (encoded-op bit-or "00000001" "0000000000000010") "0000000000000011")))
  (testing "it passes quickcheck"
    (let [results (stest/check `encoded-op {::stc/opts {:num-tests 100}})]
      (is (= (:pass? (::stc/ret (first results))) true)
          results))))
(s/def ::bitwise-function (s/with-gen
                            (s/fspec :args (s/cat :a int? :b int?) :ret int?)
                            #(gen/elements [bit-or bit-and bit-xor])))
(def encoded-individual-gen (gen/fmap clojure.string/join (gen/bind (gen/fmap #(* 8 %) (gen/fmap Math.abs gen/small-integer))
                                                                           #(gen/vector (gen/elements ["0" "1"]) %))))
(s/def ::encoded-individual (s/with-gen
                              (s/and string? #(re-matches #"^([01]{8})+$" %))
                              (fn [_] encoded-individual-gen)))
(s/fdef encoded-op
  :args (s/cat :op ::bitwise-function
               :fst ::encoded-individual
               :snd ::encoded-individual)
  :ret ::encoded-individual
  :fn #(= (count (:ret %)) (max (count (-> % :args :fst)) (count (-> % :args :snd)))))
(stest/instrument `encoded-op)

(defn pad-to-8 [encoded]
  (str (clojure.string/join (repeat (mod (- 8 (mod (count encoded) 8)) 8) "0")) encoded))
(deftest test-pad-to-8
  (testing "it pads with zeroes to a multiple of 8"
    (is (= (pad-to-8 "001") "00000001")))
  (testing "it passes quickcheck"
    (let [results (stest/check `pad-to-8 {::stc/opts {:num-tests 100}})]
      (is (= (:pass? (::stc/ret (first results))) true)
          results))))
(s/fdef pad-to-8
  :args (s/cat :encoded (s/with-gen
                          (s/and string? #(re-matches #"^[01]+$" %))
                          (fn [_] (gen/fmap clojure.string/join (gen/bind gen/small-integer #(gen/vector (gen/elements ["0" "1"]) %))))))
  :ret ::encoded-individual)

(defn encode [params]
  (->> params
       (sort-by first)
       (map (fn [[param {:keys [::min ::max ::value]}]]
              (pad-to-8 (.toString (Math.round (* 255 (/ (- value min) (- max min)))) 2))))
       (clojure.string/join)))
(s/def ::positive-number (s/and number? #(>= % 0) #(< % 10000)))
(s/def ::min ::positive-number)
(s/def ::max ::positive-number)
(s/def ::value ::positive-number)
(s/def ::param (s/and (s/keys :req [::min ::max ::value])
                      #(< (::min %) (::max %))
                      #(<= (::min %) (::value %) (::max %))))
(s/def ::individual-full-form (s/and (s/map-of keyword? ::param) #(> (count %) 0)))
(s/fdef encode
  :args (s/cat :params ::individual-full-form)
  :ret ::encoded-individual)
(stest/instrument `encode)
(deftest test-encode
  (testing "it encodes params in bytes"
    (is (= (encode {:param1 {::min 0 ::max 100 ::value 50}}) "10000000")))
  (testing "it encodes several params in bytes"
    (is (= (encode {:param1 {::min 1 ::max 11 ::value 6}
                    :param2 {::min 0 ::max 100 ::value 25}}) "1000000001000000")))
  (testing "it encodes always in the same order"
    (is (= (encode {:param1 {::min 0 ::max 100 ::value 50}
                    :param2 {::min 0 ::max 100 ::value 25}})
           (encode {:param2 {::min 0 ::max 100 ::value 25}
                    :param1 {::min 0 ::max 100 ::value 50}}))))
  (testing "it rounds down anything that's above 8 bits"
    (is (= (encode {:param1 {::min 0 ::max 100 ::value 100}})
           "11111111")))
  (testing "it passes quickcheck"
    (let [results (stest/check `encode {::stc/opts {:num-tests 100}})]
      (is (= (:pass? (::stc/ret (first results))) true)
          results))))

(defn kinda= [a b]
  (< (Math.abs (- a b)) 0.1))

(defn decode [encoded params]
  (let [sorted-params (sort-by first params)
        partitioned-bits (map #(js/parseInt (clojure.string/join %) 2) (partition 8 encoded))]
    (into {} (map (fn [[k v] bits]
                    (let [min (::min v) max (::max v)]
                      [k (assoc v ::value (+ min (* (- max min) (/ bits 0xff))))]))
                  sorted-params
                  partitioned-bits))))
(deftest test-decode
  (testing "it decodes encoded individuals into params"
    (is (= (decode "00000000" {:param1 {::min 0 ::max 100}}) {:param1 {::min 0 ::max 100 ::value 0}})))
  (testing "it isn't very precise"
    (is (kinda= ((comp ::value :param1) (decode "10000000" {:param1 {::min 0 ::max 10}})) 5))
    (is (not= ((comp ::value :param1) (decode "10000000" {:param1 {::min 0 ::max 100}})) 50)))
  (testing "it works with several several params"
    (is (kinda= ((comp ::value :p1) (decode "1000000001000000" {:p1 {::min 1 ::max 11} :p2 {::min 0 ::max 100}}))
                6))
    (is (kinda= ((comp ::value :p2) (decode "1000000001000000" {:p1 {::min 1 ::max 11} :p2 {::min 0 ::max 100}}))
                25)))
  (testing "it decodes always in the same order"
    (is (= (decode "1000110001000010" {:param1 {::min 0 ::max 100} :param2 {::min 0 ::max 100}})
           (decode "1000110001000010"  {:param2 {::min 0 ::max 100} :param1 {::min 0 ::max 100}})))))
(s/def ::param-no-value (s/and (s/keys :req [::min ::max])
                               #(< (::min %) (::max %))))
(s/def ::individual-no-value (s/and (s/map-of keyword? ::param-no-value) #(> (count %) 0)))
(s/fdef decode
  :args (s/and (s/cat :encoded ::encoded-individual :params ::individual-no-value)
               #(= (* 8 (count (:params %))) (count (:encoded %))))
  :ret ::individual-full-form)
(stest/instrument `decode)

(defn gen-mask [percentage numbits]
  (let [onesCount (Math.round (* percentage numbits))]
    (clojure.string/join (sort-by #(Math.random)
                                  (concat
                                   (repeat onesCount "1")
                                   (repeat (- numbits onesCount) "0"))))))
(deftest test-gen-mask
  (testing "it generates a random mask with the given percentage of ones"
    (is (= (count (filter #(= % "1") (gen-mask 0.5 (* 8 2)))) 8))
    (is (= (count (filter #(= % "0") (gen-mask 0.5 (* 8 2)))) 8))
    (is (= (gen-mask 1 (* 8 2)) "1111111111111111"))))

(defn mutate [individual percentage]
  (decode (encoded-op bit-xor (encode individual) (gen-mask percentage (count (encode individual))))
          individual))
(deftest test-mutate
  (let [p (fn [value min max] {::value value ::min min ::max max})
        i1 {:a (p 0.5 0.1 0.9) :b (p 0.2 0.1 0.9)}
        i2 {:a (p 0.9 0.1 0.9) :b (p 0.1 0.1 0.9) :c (p 0.2 0.1 0.9) :d (p 0.4 0.1 0.9) :e (p 0.5 0.1 0.9) :f (p 0.5 0.1 0.9) :g (p 0.5 0.1 0.9)}]
    (testing "it generates new individuals"
      (is (kinda= (get-in (mutate i1 0) [:a ::value]) 0.5))
      (is (kinda= (get-in (mutate i1 0) [:b ::value]) 0.2))
      (is (not= (mutate i1 0.5) i1)))
    (comment testing "it always generates a different value"
             (is (not= (mutate i2 0.8) (mutate i2 0.8))))))

(defn combine [individual1 individual2 percentage]
  (let [mask (gen-mask percentage (* 8 (count individual1)))
        reverse-mask (encoded-op (comp (partial bit-xor 0x01) bit-or) mask (clojure.string/join (repeat (count mask) "0")))]
    (decode (encoded-op bit-or
                        (encoded-op bit-and mask (encode individual1))
                        (encoded-op bit-and reverse-mask (encode individual2)))
            individual1)))
(deftest test-combine
  (let [p (fn [value min max] {::value value ::min min ::max max})
        i1 {:a (p 0.5 0.1 0.9) :b (p 0.2 0.1 0.9)}
        i2 {:a (p 0.9 0.1 0.9) :b (p 0.1 0.1 0.9)}]
    (testing "it combines two individuals taking a percentage of bits from individual 1 and getting the remaining from individual 2"
      (is (kinda= ((comp ::value :a) (combine i1 i1 0.5)) 0.5))
      (is (kinda= ((comp ::value :b) (combine i1 i1 0.5)) 0.2))
      (is (kinda= ((comp ::value :a) (combine i1 i2 1)) 0.5))
      (is (kinda= ((comp ::value :b) (combine i1 i2 1)) 0.2)))))

(defn breed [individual1 individual2]
  (->> [
        (mutate individual1 0.5)
        (mutate individual2 0.5)
        (combine individual1 individual2 0.5)
        (combine individual1 individual2 0.5)
        (combine individual1 individual2 0.1)
        (combine individual1 individual2 0.9)
        (mutate individual1 0.1)
        (mutate individual2 0.1)
        ]
       (map (fn [k v] [k v]) (range 8))
       (into {})))
(deftest test-breed
  (let [p (fn [value min max] {::value value ::min min ::max max})
        i1 {:a (p 0.5 0.1 0.9) :b (p 0.2 0.1 0.9)}
        i2 {:a (p 0.9 0.1 0.9) :b (p 0.1 0.1 0.9)}]
    (testing "it breeds new individuals"
      (is (= (count (breed i1 i2)) 8)))
    (testing "new individuals have the same params as breeders"
      (is (every? (fn [[k v]] (= (keys v) [:a :b])) (breed i1 i2))))
    (testing "new individuals are identified by a counter from zero to 7"
      (is (= (range 8) (sort (keys (breed i1 i2)))))) ; perfect use-case for spec. Maybe one day.
    (comment testing "none of the individuals are the same as the original ones"
      (is (every? (fn [x] (not= (second x) i1)) (breed i1 i2))) ; I hate this test, but I couldn't do it better
      (is (every? (fn [x] (not= (second x) i2)) (breed i1 i2))))))

(defn hidrate [state configs]
  (->> state
       (map (fn [[k v]] [k (assoc (get configs k) ::value v)]))
       (into {})))
(deftest test-hidrate
  (testing "it hidrates the given individual with its min and max"
    (is (= (hidrate {:a 10 :b 1} {:a {::min 0 ::max 10} :b {::min 0 ::max 9}})
           {:a {::min 0 ::max 10 ::value 10} :b {::min 0 ::max 9 ::value 1}}))))

(defn dehidrate [hidrated-state]
  (->> hidrated-state
       (map (fn [[k v]] [k (into {} (map (fn [[k v]] [k (::value v)]) v))]))
       (into {})))
(deftest test-dehidrate
  (testing "it dehidrates the given hidrated state to be put back in the app state"
    (is (= (dehidrate {1 {:a {::min 0 ::max 10 ::value 10} :b {::min 0 ::max 9 ::value 1}}
                       2 {:a {::min 0 ::max 10 ::value 4} :b {::min 0 ::max 9 ::value 2}}})
           {1 {:a 10 :b 1}
            2 {:a 4 :b 2}}))))

(defn gen-individual
  ([params] (gen-individual params Math.random))
  ([params random] (->> params
                        (map (fn [[k v]] [k (assoc v ::value (+ (::min v) (* (random) (- (::max v) (::min v)))))]))
                        (into {}))))
(deftest test-gen-individual
  (testing "it generates random individuals given params"
    (is (every? (fn [[k v]] (::value v)) (gen-individual {:param1 {::min 1 ::max 11} :param2 {::min 0 ::max 100}})))))

(def state-params-ranges
  {
   :sizeDiff {::min 1 ::max 30}
   :zoom {::min 1 ::max 7}
   :targetOccupation {::min 0.01 ::max 0.40}
   :hpoint {::min 0 ::max 360}
   :hrange {::min 0 ::max 360}
   :spoint {::min 0 ::max 99} ; tricky: can't use 100, because it wraps to 0
   :srange {::min 0 ::max 99}
   :lpoint {::min 0 ::max 99}
   :lrange {::min 0 ::max 99}
   })

(defn gen-state []
  (into {} (map (fn [x] [x (->> state-params-ranges
                                (gen-individual)
                                (map (fn [[k v]] [k (::value v)]))
                                (into {})
                                (#(assoc % :selected false)))])
                (range 8))))
(deftest test-gen-state
  (testing "it generates several configs"
    (is (= (count (gen-state)) 8)))
  (testing "all configs have a sizeDiff param"
    (is (every? (comp :sizeDiff second) (gen-state))))
  (testing "all configs have a selected param"
    (is (every? (fn [[k v]] (contains? v :selected)) (gen-state)))))

(defonce app-state (atom (gen-state)))

(defn gen-new-population [current-state]
  (let [[individual1 individual2] (take 2 (filter #(:selected (second %)) current-state))
        new-state (dehidrate (breed (hidrate (dissoc (second individual1) :selected) state-params-ranges)
                                    (hidrate (dissoc (second individual2) :selected) state-params-ranges)))]
    (->> current-state
         (map (fn [[k v]] [k (if (or (= (first individual1) k) (= (first individual2) k))
                               (assoc v :selected false)
                               (assoc (get new-state k) :selected false))]))
         (into {}))))
(deftest test-gen-new-population
  (let [sample (-> (gen-state)
                   (assoc-in  [3 :selected] true)
                   (assoc-in  [4 :selected] true)
                   (assoc-in  [5 :selected] true))]
    (testing "it generates several configs"
      (is (= (count (gen-new-population sample)) 8)))
    (testing "all configs have a sizeDiff param"
      (is (every? (comp :sizeDiff second) (gen-new-population sample))))
    (testing "all configs have a selected param"
      (is (every? (fn [[k v]] (contains? v :selected)) (gen-new-population sample))))
    (testing "it keeps only two of the selected previous ones"
      (is (every? (fn [[k v]] (if (or (= 3 k) (= 4 k))
                                (= v (assoc (get sample k) :selected false))
                                (not= v (assoc (get sample k) :selected false))))
                  (gen-new-population sample))))
    (testing "it clears the :selected flag"
      (is (not-any? (comp :selected second)
                    (gen-new-population sample))))))

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

(defn color-circle [hpoint hrange spoint srange lpoint lrange circle]
  (assoc circle :f (pick-color hpoint hrange spoint srange lpoint lrange)))

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
         p {:x (Math.round (+ r (* (random) (- width (* 2 r)))))
            :y (Math.round (+ r (* (random) (- height (* 2 r)))))}
         m (apply min (map (fn [x] (Math.floor (- (distance p (:p x)) (:r x))))
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

(defn circle-area [circle]
  (* Math.PI (:r circle) (:r circle)))

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
             :occupation (+ occupation (circle-area circle))}
            {:circles circles :occupation occupation})
          (reduced {:circles circles :occupation occupation})))
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

(defn draw-circle [circle]
  [:circle
   {
    :key (hash (str circle))
    :cx (:x (:p circle))
    :cy (:y (:p circle))
    :r (:r circle)
    :fill (:f circle)
    }])

(defn draw-range [param min max step converter-function]
  [:div {:class "range-container"}
   [:input {:type "range" ::min min ::max max :step step ::value (param @app-state) :key (hash (str "range " param)) :class "slider"
            :onChange (fn [e] (swap! app-state update-in [param] (fn [_] (converter-function (-> e .-target .-value)))))}]
   [:span nil (str param ":" (param @app-state))]])

(defn update-with-new-population [new-population]
  (swap! app-state #(identity new-population)))

(defn onclick-svg
  ([counter]
   (onclick-svg app-state counter (fn [state] (update-with-new-population (gen-new-population state)))))
  ([atom-to-update counter gen-population-callback]
   (fn [e] (let [updated (swap! atom-to-update update-in [counter :selected] (fn [x] (not x)))]
             (if (>= (count (filter (fn [[k v]] (:selected v)) updated)) 2)
               (gen-population-callback @atom-to-update))))))
(deftest test-onclick-svg
  (testing "it toggles the selected state on one item"
    (let [a (atom {0 {} 1 {} 3 {:selected false}})]
      (is (= (do ((onclick-svg a 1 (fn [])) "dummy event") @a)
             {0 {} 1 {:selected true} 3 {:selected false}})))
    (let [a (atom {1 {} 3 {:selected true}})]
      (is (= (do ((onclick-svg a 3 (fn [])) "dummy event") @a)
             {1 {} 3 {:selected false}})))
    (let [a (atom {0 {:selected false} 1 {:selected false} 3 {:selected true}})
          called-with (atom nil)]
      (testing "when two get selected, it generates new populations"
        (is (= (do
                 ((onclick-svg a 1 (fn [x] (swap! called-with (fn [_] x)))) "dummy event")
                 @called-with)
               {0 {:selected false} 1 {:selected true} 3 {:selected true}}))))))

(defn draw-svg [{:keys [sizeDiff zoom targetOccupation hpoint hrange spoint srange lpoint lrange]}]
  [:svg {:viewBox (clojure.string/join " " [0 0 width height]) :width "100%" :height "100%"}
   (->> (gen-circles sizeDiff zoom targetOccupation)
        (map #(color-circle hpoint hrange spoint srange lpoint lrange %))
        (map draw-circle))])

(defn draw-svg-container [counter state]
  [:div {:class (str "svg-container" (if (:selected state) " selected"))
         :onClick (onclick-svg counter)}
   [draw-svg (dissoc state :selected)]
   [:div {:class (str "svg-container-overlay" (if (:selected state) " selected"))}
    [:img {:src "star.svg"}]]])

(defn sparkline-data [state configs]
  (map (fn [[k v]] (/ (::value v) (- (::max v) (::min v))))
       (sort (hidrate (filter (fn [[k v]] (not= k :selected)) state) configs))))
(deftest test-sparkline-data
  (testing "it outputs a list of percentage sparklines"
    (is (= (sparkline-data {:a 10 :b 2 :selected true} {:a {::min 0 ::max 10} :b {::min 0 ::max 8}})
           [1 (/ 1 4)])))
  (testing "it cleans up the selected flag from the items"
    (is (= (count (sparkline-data {:a 10 :b 2 :selected true} {:a {::min 0 ::max 10} :b {::min 0 ::max 8}}))
           2))))

(def sparkline-width 6)
(def sparkline-height 40)

(defn draw-rect [el i x]
  [:rect {:key (hash (str "rect " el i x))
          :x (* i sparkline-width)
          :y (- (* x sparkline-height))
          :width sparkline-width
          :height (* x sparkline-height)
          :style {:fill "rgb(200,200,200)"}}])

(defn draw-sparkline [state]
  [:div {:class "sparklines-container"}
   (map (fn [[k state-item]]
          [:svg {:key (hash (str "sparkline " k))
                 :viewBox (str "0 -" sparkline-height " " (* sparkline-width (count state)) " " sparkline-height)
                 :width "100%"
                 :height "100%"}
           (map-indexed (fn [i x] (draw-rect k i x))
                        (sparkline-data state-item state-params-ranges))])
        state)])

(defn main-render []
  [:div
   [:div {:class "flex-container"}
    [:p nil "Select the two images you like the most to seed a new generation. Sparklines at the top show the configs of each image, and you should see them converge as you go through generations."]]
   [:div {:class "flex-container"}
    ;[:button {:onClick #(update-with-new-population (gen-new-population @app-state))} "New population"]
                                        ;[:button {:onClick #(println @app-state)} "Dump state"]
    ]
   [:div
    [draw-sparkline @app-state]]
   [:div {:class "flex-container main-section"}
    [draw-svg-container 0 (get @app-state 0)]
    [draw-svg-container 1 (get @app-state 1)]
    [draw-svg-container 2 (get @app-state 2)]
    [draw-svg-container 3 (get @app-state 3)]
    [draw-svg-container 4 (get @app-state 4)]
    [draw-svg-container 5 (get @app-state 5)]
    [draw-svg-container 6 (get @app-state 6)]
    [draw-svg-container 7 (get @app-state 7)]]
   [:div {:class "flex-container"}
    ;[:button {:onClick #(update-with-new-population (gen-new-population @app-state))} "New population"]
    [:button {:onClick #(println @app-state)} "Debug: Dump state"]]
   ])

(reagent/render-component [main-render]
                          (. js/document (getElementById "app")))

(cljs.test/run-tests)
