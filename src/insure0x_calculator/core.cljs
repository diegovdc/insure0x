(ns insure0x-calculator.core
  (:require
   [reagent.core :as r]
   [insure0x-calculator.calculations :as calc]
   [oz.core :as oz]
   [reagent.dom :as d]
   [cljs.core.async :as a]))

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

(defonce state (r/atom {:total-beneficiaries 20
                        :fee 100
                        :estimated-risk 0.02
                        :min-amount 3000
                        :max-amount 5000
                        :total-months 12
                        :total-simulations 50}))
(defonce result-data (r/atom nil))
(defonce raw-data (r/atom nil))
(defonce calculating? (r/atom false))
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

(comment
  (->> @raw-data
       (map :served)
       (map frequencies)
       (apply merge-with +)))

(defn run-simulation []
  (reset! calculating? true)
  (a/go
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
      (reset! raw-data data)
      (reset! result-data
              {:total-simulations total-simulations
               :total-benficiaries (* beneficiaries (count data))
               :total-unserved (->> data (map :unserved) (apply +))
               :total-served (->> data (mapcat :served) frequencies)
               :savings (->> data (map :reserve)
                             (map (partial calc/round-val 10000))
                             frequencies (sort-by first))
               :max-amount (->> data first :maximum-amount)
               :unserved (->> data (map :unserved) frequencies (sort-by first))
               :partially-served (->> data
                                      (map :served)
                                      (map (comp count
                                                 (partial filter #(= :partial %))))
                                      frequencies
                                      (sort-by first))
               :served-vs-partial-vs-unserved (->> data
                                                   (mapcat :served)
                                                   frequencies)
               :partial-cash-unserved (->> data
                                           (mapcat :partial-cash-unserved)
                                           (map (partial calc/round-val 1000))
                                           frequencies (sort-by first))})
      (reset! calculating? false))))
(def formatter (js/Intl.NumberFormat "en-US" (clj->js {"style" "currency" "currency" "USD"})))





(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :quantity (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))


(defn get-% [part total] (* 100 (/ part total)))

(defn home-page []
  [:div
   [:h1 "Insure0x Calculator"]
   (num-input :total-beneficiaries)
   (num-input :fee)
   (num-input :estimated-risk)
   (num-input :min-amount)
   (num-input :max-amount)
   (num-input :total-months)
   (num-input :total-simulations)
   [:button {:on-click run-simulation} "Run Simulation"]
   (if @calculating?
     [:div "Running simulation..."]
     (when @result-data
       [:div
        ;; NOTE uncomment to print data
        #_(map (fn [[k v]] [:div {:key k} (str k ": " v) ]) @result-data)

        [oz/vega-lite
         {:data {:values (map (fn [[amount total]]
                                {:total-savings (.format formatter amount)
                                 :cases-% (get-% total (:total-simulations @result-data))})
                              (:savings @result-data))}
          :encoding {:x {:field "total-savings" :type "nominal"}
                     :y {:field "cases-%" :type "quantitative"}}
          :mark "bar"}]
        [oz/vega-lite
         {:data {:values (map (fn [[amount total]]
                                {:partially-served-beneficiaries amount
                                 :cases-% (get-% total (:total-simulations @result-data))})
                              (:partially-served @result-data))}
          :encoding {:x {:field "partially-served-beneficiaries" :type "nominal"}
                     :y {:field "cases-%" :type "quantitative"}}
          :mark "bar"}]
        (let [data (:partial-cash-unserved @result-data)
              total-unserved-cases (apply + (map second data))]
          (when (seq data)
            [oz/vega-lite
             {:data {:values
                     (map (fn [[amount total]]
                            {:partial-cash-unserved (.format formatter amount)
                             :cases-% (get-% total total-unserved-cases)})
                          data)}
              :encoding {:x {:field "partial-cash-unserved" :type "nominal"}
                         :y {:field "cases-%" :type "quantitative"}}
              :mark "bar"}]))
        [oz/vega-lite
         {:data {:values (map (fn [[amount total]]
                                {:unserved-beneficiaries amount
                                 :cases-% (get-% total (:total-simulations @result-data))})
                              (:unserved @result-data))}
          :encoding {:x {:field "unserved-beneficiaries" :type "nominal"}
                     :y {:field "cases-%" :type "quantitative"}}
          :mark "bar"}]
        [oz/vega-lite
         {:data {:values (map (fn [[type total]]
                                {:fully-served-vs-partial-vs-unserved type
                                 :total total})
                              (:served-vs-partial-vs-unserved @result-data))}
          :encoding {:x {:field "fully-served-vs-partial-vs-unserved" :type "nominal"}
                     :y {:field "total" :type "quantitative"}}
          :mark "bar"}]]))])


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
