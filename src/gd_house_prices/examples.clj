(ns gd-house-prices.examples
  (:require [gd-house-prices.data :refer :all]
            [clj-time.coerce :as coerce]
            [clj-time.core :as time]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.svg :as svg]))

(defn sqr [x] (* x x))
(defn y [b m x] (+ (* m x) b))

(defn error-for-line-given-parameters [b m points]
  (let [ ef (fn [p] (- (second p) (y b m (first p))))
         total (reduce + (map sqr (map ef points)))
         c     (count points)]
    ;(println "error total = " total " num points = " c)
    (/ total c)))

(defn step-gradient [b-cur m-cur points learning-rate]
  (let [fn-y       (fn [p] (+ (* m-cur (first p)) b-cur))
        fn-e       (fn [p] (- (second p) (fn-y p)))
        fn-db-comp (fn [p] (fn-e p))
        fn-dm-comp (fn [p] (* (first p) (fn-e p)))
        N          (count points)
        db         (* (/ -2 N) (reduce + (map fn-db-comp points)))
        dm         (* (/ -2 N) (reduce + (map fn-dm-comp points)))
        b-new      (- b-cur (* learning-rate db))
        m-new      (- m-cur (* learning-rate dm))]
    (list b-new m-new)))

(defn print-stats [lbl b-cur m-cur points]
  (println lbl " b = " b-cur " and m = " m-cur " and error = " (error-for-line-given-parameters b-cur m-cur points)))

(defn normalize [d]
  (println d)
  (let [c   (count d)
        m   (/ (reduce + d) c)
        sd  (Math/sqrt (/ (reduce + (map (fn [v] (sqr (- v m) )) d)) c))]
    [ (map (fn [v] (/ (- v m) sd)) d) m sd]))
       
(defn run-gradient-descent [b-start m-start points learning-rate n-iter]
  (let [i n-iter]
    (loop [ndx i
           b-cur b-start
           m-cur m-start
           c 0
           gradient-points (list [b-cur m-cur])
           error-points (list [0 (error-for-line-given-parameters b-cur m-cur points)])]
      (if (> ndx 0)
        (let [ x (step-gradient b-cur m-cur points learning-rate)
               b-new (first x)
               m-new (last x) ]
          (if (= c 0) (print-stats "Starting gradient descent with " b-cur m-cur points))
          (if (= 0 (mod c 10)) (println "at " c ": " x))
          (recur (- ndx 1) b-new m-new (+ c 1) (conj gradient-points [b-new m-new])
            (conj error-points [(+ c 1) (error-for-line-given-parameters b-new m-new points)])
                ))
        (let []
          (print-stats (str "After " c " iterations")  b-cur  m-cur points)
          (list b-cur m-cur gradient-points error-points))))))

(defn ex-1-0 []
  (let [ data-orig (:rows (house-prices-data))
         areas (map (fn [p] (:Area p)) data-orig)
         na-r (normalize areas)
         normal-areas (first na-r)
         areas-mean (second na-r)
         areas-std (nth na-r 2)
         prices (map (fn [p] (:Price p)) data-orig)
         np-r (normalize prices)
         normal-prices (first np-r)
         prices-mean (second np-r)
         prices-std (nth np-r 2)
         points (zipmap normal-areas normal-prices)
         x areas
         y prices
         r (run-gradient-descent 0 0 points 0.01 150)
         b-cur (first r)
         m-cur (second r)
         normalize-area (fn [a] (/ (- a areas-mean) areas-std))
         denormalize-price (fn [p] (+ (* p prices-std) prices-mean))
         x->y  (fn [v] (denormalize-price (+ (* m-cur (normalize-area v)) b-cur)))
         gradient-points (take-nth 5 (reverse (nth r 2)))
         gx (map first gradient-points)
         gy (map second gradient-points)
         error-points (take-nth 5 (drop 20 (reverse (nth r 3))))
         ex (map first error-points)
         ey (map second error-points)]
    (-> (c/scatter-plot x y :x-label "area in sq ft" :y-label "price")
      (c/add-function x->y 500 5000 :x-label "area in sq ft" :y-label "price")
      (i/view))
    (-> (c/scatter-plot gx gy :x-label "line y-intercept (B)" :y-label "line slope (M)")
      (i/view))
    (-> (c/scatter-plot ex ey :x-label "Number of iterations" :y-label "error") (i/view))))
