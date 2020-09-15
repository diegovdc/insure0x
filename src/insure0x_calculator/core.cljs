(ns insure0x-calculator.core
  (:require
   [reagent.core :as r]
   [insure0x-calculator.calculations :as calc]
   [oz.core :as oz]
   [reagent.dom :as d]))

(def beneficiaries 1000)

#_(def test-data
    (mapv (fn [_]
            (let [benefs beneficiaries
                  fee 100
                  need%  0.01
                  config {:min-amount 2000 :growth-rate 0.005}
                  fund {:reserve 0
                        :fee fee
                        :maximum-amount 15000
                        :period 180
                        :beneficiaries (calc/init-beneficiaries benefs fee need%)
                        :unserved 0}
                  #_(init-fund {:beneficiaries (init-beneficiaries benefs fee need%)
                                :fee fee
                                :maximum-amount 10000
                                :period 180})]
              (calc/run-months fund config 48)))
          (range 100)))



;; -------------------------
;; Views

(def state (r/atom {:total-beneficiaries 100
                  :fee 100
                  :estimated-risk 0.01
                  :min-amount 1000
                  :max-amount 10000
                  :total-months 12
                  :total-simulations 50}))
(defonce result-data (r/atom nil))

(defn num-input
  [key]
  [:div
   [:label (str (name key) "  ")
    [:input {:type "number"
             :id key
             :default-value (@state key)
             :on-change (fn [ev] (swap! state
                                       assoc
                                       key
                                       (-> ev .-target .-value js/Number)))}]]])

(comment (run-simulation))

(defn run-simulation []
  (let [{:keys [total-beneficiaries fee estimated-risk
                min-amount max-amount total-simulations]} @state
        data (mapv (fn [_]
                     (let [config {:min-amount min-amount :growth-rate 0}
                           fund {:reserve 0
                                 :fee fee
                                 :maximum-amount max-amount
                                 :period 180
                                 :beneficiaries (calc/init-beneficiaries
                                                 total-beneficiaries fee estimated-risk)
                                 :unserved 0}]
                       (calc/run-months fund config 48)))
                   (range total-simulations))]
    (reset! result-data
            {:total-examples (count data)
             :total-benficiaries (* beneficiaries (count data))
             :total-unserved (->> data (map :unserved) (apply +))
             :total-served (->> data (mapcat :served) frequencies)
             :min-max (->> data (map :reserve)
                           (map (partial calc/round-val 10000))
                           frequencies (sort-by first))
             :max-amount (->> data first :maximum-amount)
             :unserved (->> data (map :unserved) frequencies (sort-by first))
             :partially-served (->> data (map (comp #(or % 0) :partially-served))
                                    frequencies (sort-by second))
             :partial-cash-unserved (->> data
                                         (mapcat :partial-cash-unserved)
                                         (map (partial calc/round-val 1000))
                                         frequencies (sort-by first))})))
(def formatter (js/Intl.NumberFormat "en-US" (clj->js {"style" "currency" "currency" "USD"})))





(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :quantity (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))

(play-data "monkey" "slipper" "broom")

(defn home-page []
  [:div
   [:h2 "Insure0x Calculator"]
   (num-input :total-beneficiaries)
   (num-input :fee)
   (num-input :estimated-risk)
   (num-input :min-amount)
   (num-input :max-amount)
   (num-input :total-months)
   (num-input :total-simulations)
   [:button {:on-click run-simulation} "Run Simulation"]
   (when @result-data
     [:div (map (fn [[k v]] [:div {:key k} (str k ": " v) ]) @result-data)
      [:div
       [oz/vega-lite
        #_{:data {:values (play-data "monkey" "slipper" "broom")}
         :encoding {:x {:field "time" :type "quantitative"}
                    :y {:field "quantity" :type "quantitative"}
                    ;; :color {:field "item" :type "nominal"}
                    }
         :mark "bar"}
        {:data {:values (map (fn [[amount total]]
                               {:amount (.format formatter amount)
                                :total total})
                             (:min-max @result-data))}
         :encoding {:x {:field "amount" :type "nominal"}
                    :y {:field "total" :type "quantitative"}}
         :mark "bar"
         :width 500
         :height 400}]]])
   ])


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
