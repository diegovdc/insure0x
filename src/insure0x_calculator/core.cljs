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
  ([key] (num-input key nil))
  ([key comment]
   [:div
    [:label [:b (str (name key) "  ")]
     [:small comment]
     [:input {:type "number"
              :id key
              :default-value (@state key)
              :on-change (fn [ev] (swap! state
                                        assoc
                                        key
                                        (-> ev .-target .-value js/Number)))}]]]))

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
   [:h1 {:style {:textAlign "center" :marginBottom "50px"}} "Community Driven Microinsurance Calculator"]
   [:section
    ;; simulate accidents at random given a probability `risk`
    [:h2 "What is this?"]
    [:p "The purpose of this calculator is to make it easy to design community backed custom insurance policies for small groups of people (e.g guilds) that for any reason have no access to traditional insurance services. By playing with a few basic parameters and running simulations this tools allows us to discover how a policy will behave over time, what are it's benefits, it's risks and how it could be optimized."]
    [:p "The main assumption of the policies that can be designed with this tool is that the coverage will be somewhat limited, both in the amounts of money that can be provided and in the cases tha may be covered by it (the later aspect, highly related to " [:b "risk"] " assesment is yet to be worked out for the current version of this tool)."]
    [:h2 "How does this calculator work?"]
    [:p "Because accidents that can be covered by an insurance most often happen at random. This calculator functions by simulating accidents given the probability of them ocurring once a month. This probability is defined by the " [:b "estimated-risk"] " parameter in the form below."]
    [:p "To calculate how much money, per accident, can an insurace policy cover, we need to know how much money is available. The total funds are calculated by multiplying a yearly " [:b "fee"] " by the total number of insured persons, that is, the " [:b "beneficiaries"] " of the policy."]
    [:p "Then, we can play around with the estimated range of costs (" [:b "max-amount"] " and " [:b "min-amount"] ") our policy will be able to cover. It is important to note that for the purposes of the simulation, when an accident \"occurs\" it's cost will be chosen at random from within this range. Exploring these values will allow us to manually optimize the policy for different cases, and to find out what are the limits of the coverage given the total amount of beneficiaries, the yearly fee and the estimated risk."]
    [:p "Having all the parameters set we can run a " [:b "simulation"] " to see how the policy will behave. But running a single simulation will not tell us as much as we need to know (accidents are random occurences). For example, what if we got lucky in the simulation and nobody had any accidents? Or, on the contrary, what if an abnormal amount of people required the insurance service and we ran out funds? To make sure that our policy is solid, we need to run many simulations so that we can know which cases will be the most common, and which is the risk that the policy will not be able to cover someone's expenses (either partially or in full). The " [:b "total-simulations"] " parameter in the form below will allow us to do just that; but bear in mind that running many simulations will take some time, so you may want to test at first with just a few and then, when you find something you like, set a higher value to make sure everything still works. There is also a " [:b "total-months"] " parameter that will define how much time will the simulations contemplate."]
    [:h2 "Metrics"]
    [:p "The simulations will produce a set of graphs for the following metrics:"]
    [:h3 "1. Total savings"]
    [:p "The total amount of money left after the simulation. Each column represents the probability of having a certain amount of savings around a certain range. Please note that the amplitud of the range is $10,000, so a column at $20,000, represents savings between $15,001 and $25,000."]
    [:h3 "2. Partially served beneficiaries"]
    [:p "If the funds almost run out, there is a chance that a beneficiary will only be able to receive a partial refund. This graph represents the probability of such an event happening."]
    [:h3 "3. Unserved beneficiaries"]
    [:p "If the funds almost out and there is an accident, the beneficiary will not be able to receive any refund. This graph represents the probability of such an event happening."]
    [:h3 "4. Fully served vs partially served vs unserved beneficiaries"]
    [:p "This graph shows the percentages of beneficiaries in each of the categories after the simulations"]]
   [:section {:style {:marginBottom "120px"}}
    [:h2 "Calculator"]
    (num-input :total-beneficiaries)
    (num-input :fee "(per month but paid yearly)")
    (num-input :estimated-risk "(monthly probability -from 0 to 1- that the insurance will be required)")
    (num-input :min-amount "(coverage range)")
    (num-input :max-amount "(coverage range)")
    (num-input :total-months "(to run simulation)")
    (num-input :total-simulations "(more simulations give more precise estimates but are slower to run)")
    [:button {:on-click run-simulation} "Run Simulation"]
    (if @calculating?
      [:div "Running simulation..."]
      (when @result-data
        [:div
         ;; NOTE uncomment to print data
         #_(map (fn [[k v]] [:div {:key k} (str k ": " v) ]) @result-data)
         [:span
          [oz/vega-lite
           {:data {:values (map (fn [[amount total]]
                                  {:total-savings (.format formatter amount)
                                   :cases-% (get-% total (:total-simulations @result-data))})
                                (:savings @result-data))}
            :encoding {:x {:field "total-savings" :type "nominal"}
                       :y {:field "cases-%" :type "quantitative"}}
            :mark "bar"}]]
         [oz/vega-lite
          {:data {:values (map (fn [[amount total]]
                                 {:partially-served-beneficiaries amount
                                  :cases-% (get-% total (:total-simulations @result-data))})
                               (:partially-served @result-data))}
           :encoding {:x {:field "partially-served-beneficiaries" :type "nominal"}
                      :y {:field "cases-%" :type "quantitative"}}
           :mark "bar"}]
         ;; TODO improve this graph
         #_(let [data (:partial-cash-unserved @result-data)
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
         (let [data* (:served-vs-partial-vs-unserved @result-data)
               total-beneficiaries (apply + (map second data*))]
           (println total-beneficiaries)
           [oz/vega-lite
            {:data {:values (map (fn [[type total]]
                                   {:fully-served-vs-partial-vs-unserved type
                                    :cases-% (get-% total total-beneficiaries)})
                                 data*)}
             :encoding {:x {:field "fully-served-vs-partial-vs-unserved" :type "nominal"}
                        :y {:field "cases-%" :type "quantitative"}}
             :mark "bar"}])]))]])


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
