(ns app.verse
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]))

;; State atom for the verse data
(def verse-state
  (r/atom
   {:text "Salvātor mundī, postquam dē Virgine nāscī"
    :marks [{:type :long :start 0 :end 2}
            {:type :long :start 3 :end 4}
            {:type :long :start 5 :end 7}
            {:type :long :start 9 :end 12}
            {:type :short :start 31 :end 32}
            {:type :short :start 33 :end 34}
            ]}))

;; line->svg component with precise measurements
(defn line->svg [{:keys [text marks]}]
  (r/with-let [font-size 16
               y-text 20     ; Y-position of text
               y-mark 30     ; Y-position for marks baseline
               arc-height 8  ; Height of short mark arc
               height 50     ; SVG height
               ;; State for dynamically measured mark positions
               mark-positions (r/atom {})
               ;; Ref to track if measurements have been taken for the current marks
               measured-marks-ref (r/atom nil)
               ]
    (let [measure-marks
          (fn []
            (println "measure-marks" @measured-marks-ref marks)
            (when (not= @measured-marks-ref marks)
              (when-let [main-text (.getElementById js/document "verse-text")]
                (let [svg (.getElementById js/document "verse-svg")
                      text-width (.-width (.getBBox main-text))]
                  (.setAttribute svg "width" text-width)))
              (doseq [[idx {:keys [start end]}] (map-indexed vector marks)]
                (println "Measuring: " idx)
                (let [start-text (.getElementById js/document (str "measure-start-" idx))
                      end-text (.getElementById js/document (str "measure-end-" idx))
                      x1 (if start-text (.-width (.getBBox start-text)) 0)
                      x2 (if end-text (.-width (.getBBox end-text)) 0)]
                  (swap! mark-positions assoc idx {:x1 x1 :x2 x2})))
              (reset! measured-marks-ref marks)
              (println @measured-marks-ref)))]
      ;; Use after-render to trigger measurements
      (when (not= @measured-marks-ref marks)
        (r/after-render measure-marks))
      [:div
       [:p "mark-positions" @mark-positions]
       [:svg
        {:id "verse-svg" :height height :xmlns "http://www.w3.org/2000/svg"}
        [:g
         ;; Main text
         [:text {:id "verse-text" :x 0 :y y-text :font-family "monospace" :font-size font-size}
          text]
         ;; Hidden text elements and marks
         (doall
          (map-indexed
           (fn [idx {:keys [type start end] :as mark}]
             (let [prefix-start (subs text 0 start)
                   prefix-end (subs text 0 end)
                   {:keys [x1 x2]} (get @mark-positions idx {:x1 0 :x2 0})]
               [:<> {:key (str "mark-group-" idx)}
                ;; Hidden text for measuring start and end
                [:text {:id (str "measure-start-" idx) :visibility "hidden"
                        :font-family "monospace" :font-size font-size}
                 prefix-start]
                [:text {:id (str "measure-end-" idx) :visibility "hidden"
                        :font-family "monospace" :font-size font-size}
                 prefix-end]
                ;; Mark element
                (cond
                  (= type :long)
                  [:line {:key (str "mark-" idx)
                          :x1 x1 :y1 y-mark :x2 x2 :y2 y-mark
                          :stroke "black" :stroke-width 1}]
                  (= type :short)
                  (let [mid-x (/ (+ x1 x2) 2)
                        control-y (+ y-mark arc-height)]
                    [:path {:key (str "mark-" idx)
                            :d (str "M" x1 "," y-mark " Q" mid-x "," control-y " " x2 "," y-mark)
                            :fill "none" :stroke "black" :stroke-width 1}])
                  :else nil)]))
           marks))]]])))

;; Component to handle mark addition
(defn mark-adder []
  (r/with-let
    [mark-type (r/atom :long)         ; Default mark type
     selection-range (r/atom nil)]     ; Selection state
    (let [update-selection
          (fn []
            (let [sel (js/window.getSelection)
                  range (when (.-rangeCount sel) (.getRangeAt sel 0))
                  text-node (.getElementById js/document "verse-text")
                  text (:text @verse-state)]
              (when (and range text-node)
                ;; Check if the selection is within the verse-text element
                (let [start-container (.-startContainer range)
                      end-container (.-endContainer range)
                      is-within-text-node
                      (or (identical? start-container text-node)
                          (.contains text-node start-container)
                          (identical? end-container text-node)
                          (.contains text-node end-container))]
                  (when is-within-text-node
                    (let [selected-text (str sel)
                          start (.indexOf text selected-text)]
                      (when (and (>= start 0) (not= start -1))
                        (let [end (+ start (.-length selected-text))]
                          (when (<= end (count text))
                            (reset! selection-range {:start start :end end}))))))))))
          add-mark
          (fn []
            (when @selection-range
              (swap! verse-state update :marks conj
                     (assoc @selection-range :type @mark-type))
              (reset! selection-range nil)
              (.removeAllRanges (js/window.getSelection))))]
      [:div
       [:h3 "Add Mark"]
       [:select {:value @mark-type
                 :on-change #(reset! mark-type (keyword (-> % .-target .-value)))}
        [:option {:value "long"} "Long"]
        [:option {:value "short"} "Short"]]
       [:button {:on-click #(do (update-selection) (add-mark))} "Add Mark"]
       (when @selection-range
         [:p "Selected: " (str @selection-range)])])))

;; Main verse component
(defn verse []
  [:<>
   [:h1 "Text with Versification Marks"]
   [line->svg @verse-state]
   [mark-adder]
   [:p (str @verse-state)]])
