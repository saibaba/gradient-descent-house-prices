(ns gd-house-prices.data
  (:require [incanter.core :as i]
            [incanter.io :as io]))

(defn house-prices-data []
  (-> (io/read-dataset "data/data.csv" :header true)))

