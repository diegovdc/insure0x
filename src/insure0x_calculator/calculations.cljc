(ns insure0x-calculator.calculations
  (:require [clojure.spec.alpha :as s]))

(s/def ::percentage (s/and number? #(>= 100 % 0)))
(s/def ::expense (s/and number? #(> % 0)))
(s/def ::days (s/and int? #(> % 0)))
(s/def ::months (s/and int? #(> % 0)))
(s/def ::need% ::percentage)
(s/def ::total-times-required (s/and int? #(> % 0)))
(s/def ::total-expenses ::expense)
(s/def ::covered-expenses ::expense)
(s/def ::uncovered-expenses ::expense)
(s/def ::beneficiary (s/keys :req-un [::need% ::total-expenses ::total-times-required
                                      ::covered-expenses ::uncovered-expenses ::months]))
(s/def ::beneficiaries (s/* ::beneficiary))
(s/def ::fee (s/and number? #(> % 0)))
(s/def ::reserve (s/and number? #(>= % 0)))
(s/def ::maximum-amount ::expense)
(s/def ::period ::days)
(s/def ::fund (s/keys :req-un [::reserve ::total-beneficiaries ::maximum-amount
                               ::period ::beneficiaries]))


(defn init-fund [{:keys [beneficiaries fee maximum-amount period]}]
  {:reserve (* (count beneficiaries) fee)
   :fee fee
   :maximum-amount maximum-amount
   :period period
   :beneficiaries beneficiaries
   :unserved 0})

(defn init-beneficiary [fee need]
  {:need% need
   :total-expenses 0
   :total-expenses-in-period 0
   :total-times-required 0
   :covered-expenses 0
   :uncovered-expenses 0
   :months 1
   :total-invested fee})

(defn init-beneficiaries [total fee need]
  (take total (map (fn [_] (init-beneficiary fee need)) (range))))

(defn rand-range [min max] (int (+ min (rand (- max min)))))

(rand-range 200 1000)


(defn update-expenses [benficiary amount]
  (-> benficiary
      (update :total-expenses + amount)
      (update :total-expenses-in-period + amount)))

(defn exceeded-coverage? [beneficiary max-amount]
  (> (beneficiary :total-expenses-in-period) max-amount))

;; TODO find out partial served
;; TODO limit max amount per beneficiary per period
;; TODO
(defn simulate-usage [{:keys [reserve beneficiaries maximum-amount] :as fund}
                      {:keys [min-amount] :as config}]
  (merge fund
         (reduce (fn [{:keys [reserve beneficiaries unserved
                             served partial-cash-unserved]} benef]
                   (let [needed? (>= (benef :need%) (rand))]
                     (cond
                       (or (not needed?) (exceeded-coverage? benef maximum-amount))
                       {:reserve reserve
                        :beneficiaries (conj beneficiaries benef)
                        :unserved unserved
                        :served served
                        :partial-cash-unserved partial-cash-unserved}
                       (= 0 reserve) {:reserve reserve
                                      :unserved (inc unserved)
                                      :beneficiaries (conj beneficiaries benef)
                                      :served served
                                      :partial-cash-unserved partial-cash-unserved}
                       (and (> reserve 0) needed?)
                       (let [required-amount (rand-range min-amount maximum-amount)
                             amount (if (>= (- reserve required-amount) 0) required-amount reserve)
                             unserved-cash (- required-amount amount)]
                         {:reserve (- reserve amount)
                          :unserved unserved
                          :beneficiaries (conj beneficiaries (update-expenses benef amount))
                          :served (conj served (if (> unserved-cash 0) :partial :total))
                          :partial-cash-unserved (if (> unserved-cash 0)
                                                   (conj partial-cash-unserved unserved-cash)
                                                   partial-cash-unserved)}))))
                 {:reserve reserve
                  :benficiaries []
                  :unserved 0
                  :served []
                  :partial-cash-unserved []}
                 beneficiaries)))

#_(let [benefs 20
        fee 50
        need%  0.2
        fund (init-fund
              {:beneficiaries (init-beneficiaries benefs fee need%)
               :fee fee
               :maximum-amount 2000
               :period 180})]
    (:unserved (run-month fund {:min-amount 200})))



(defn run-month [{:keys [beneficiaries fee] :as fund} config month]
  (-> fund
      (update :reserve #(+ % (when (= 0 (mod month 12)) (* (count beneficiaries) fee 12))))
      (simulate-usage config)
      #_(update :maximum-amount #(+ % (* % (config :growth-rate))))))

(defn round-val [rounding-quotient val]
  (let [min* (* rounding-quotient (quot val rounding-quotient))
        midpoint (/ rounding-quotient 2)]
    (if (> val (+ midpoint min*))
      (+ min* rounding-quotient)
      min*)))

(defn run-months [fund config quantity]
    (reduce (fn [fund* month] (run-month fund* config month)) fund (range quantity)))
