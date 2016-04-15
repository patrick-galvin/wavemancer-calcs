(ns wavemancer.calcs
  (:require [clojure.math.numeric-tower :as math]))

(defn bar-vals [bars key]
  (map #(get % key) bars))

(defn average
  "Average sequence of numbers"
  [vals period]
  {:pre [(pos? period)]}
  (/ (apply + (take period (seq vals))) period))

(defn merge-pairs
"Compares the [n] [n+1] bars in the collection"
  [vals]
  (loop [v (first vals) r (rest vals) total []]
    (if-let [s (first r)] (recur s (rest r) (conj total [v s]))
            total)))

(defn ema-basic-alpha
  [days]
  (/ 2 (+ days 1)))

(defn  ema-calc
  [today yesterday alpha]
  (+ (* today alpha) (* yesterday (- 1 alpha))))

(defn ema-with-alpha-f
  ([vals period f]
   (lazy-seq
    (when-let [s (seq vals)]
      (cons (first s) (ema-with-alpha-f (rest s) period (f period) (first s))))))
  ([vals period alpha previous]
  (lazy-seq
   (when-let [s (seq vals)]
     (let [ema-r (ema-calc (first s) previous alpha)]
       (cons ema-r (ema-with-alpha-f (rest s) period alpha ema-r)))))))

(defn ema
  [vals period]
  (ema-with-alpha-f vals period ema-basic-alpha))

(defn direction [vals initial-bar period]
  (math/abs (- (vals initial-bar) (vals period))))

(defn volatility [vals]
  (reduce + (map
             #(math/abs (- (first %) (second %)))
             (merge-pairs vals))))

(defn efficiency-ratio [vals period]
  (/ (direction vals 0 period) (volatility vals)))

(defn ama
  "Calculate AMA"
  ; TODO(pgalvin) finish
  ; α = [(VI * (FC – SC)) + SC] ²
  ; EMA calc
  [vals ratio-length fast-period slow-period]
  (let [f-alpha (ema-basic-alpha fast-period)
        s-alpha (ema-basic-alpha slow-period)]
    (dotimes [n ratio-length]))
  (/ (direction vals 0 ratio-length) (volatility vals)))

(defn summation
  "The Summation function adds a series of numbers together over a specified number of bars."
  [values period]
  {:pre (pos? period)}
  (reduce + (take period (seq values))))

(defn gdema
  "Calculates GDEMA using EMA base"
  ; GDEMA = [EMA * (1 + V)] – [EMA(EMA) * V]
  ; gDEMA = (1+VFactor) * MA1 - VFactor * MA2;
  [values period v-factor]
  (let [ma1 (ema values period)
        ma2 (ema ma1 period)
        ma1-v (map #(* (+ 1 v-factor) %) ma1)
        ma2-v (map #(* v-factor %) ma2)
        ]
    (map #(- %1 %2) ma1 ma2)))

(defn tri-smoother
  "Tri-smoother using GDEMA"
  ; T3 = gDEMA(gDEMA(gDEMA( Price, Period, VFactor ),Period,VFactor),Period,VFactor);
  [values period v-factor]
  {:pre (pos? period)}
  (-> (gdema values period v-factor)
      (gdema period v-factor)
      (gdema period v-factor)))

(defn tri-average
  "Tri-average"
  ; var0 = Ceiling( ( Len + 1 ) * .5 ) ;
  ; TriAverage = Average( Average( PriceValue, var0 ), var0 ) ;
  [values period]
  {:pre (pos? period)}
  (let [var0 (math/ceil (* 0.5 (+ 1 period))) p1 (average values var0)]
    (average values p1)))

