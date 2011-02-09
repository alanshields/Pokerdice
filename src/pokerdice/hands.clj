(ns pokerdice.hands
  "Functions for dealing with different hands in pokerdice"
  (:use clojure.core
        pokerdice.constants))

(defn histogram [arr]
  "Given an iterable, compute a histogram as a vector"
  (let [freqs (frequencies arr)]
    (map #(get freqs %1 0)
         (range 1 (inc +maxface+)))))

(defstruct handinfo-struct :hand :histo :histohisto)
(defn handinfo [hand]
  (let [histo (histogram hand)]
    (struct-map handinfo-struct
      :hand hand
      :histo histo
      :histohisto (histogram histo))))

(defn- ath [coll i]
  (nth coll (dec i)))

(defn five-of-a-kind? [h]
  (= 1 (ath (:histohisto h) 5)))
(defn four-of-a-kind? [h]
  (= 1 (ath (:histohisto h) 4)))
(defn full-house? [h]
  (and
   (= 1 (ath (:histohisto h) 2))
   (= 1 (ath (:histohisto h) 3))))
(defn straight? [h]
  "Check for a straight hand - FIXME: depends on maxfaces 6 and a hand size of 5"
  (and
   (= 5 (ath (:histohisto h) 1))
   (or (= 0 (ath (:histo h) 1))
       (= 0 (ath (:histo h) 6)))))
(defn three-of-a-kind? [h]
  (= 1 (ath (:histohisto h) 3)))
(defn two-pair? [h]
  (= 2 (ath (:histohisto h) 2)))
(defn pair? [h]
  (= 1 (ath (:histohisto h) 2)))
(defn nothing? [h]
  true)

(def +hand-pred+
  (zipmap +hand-order+
          (map #(resolve (symbol (str (name %1) "?")))
               +hand-order+)))
(def +hand-beats+
  (zipmap +hand-order+
          (map #(set (nthnext +hand-order+ (inc %1)))
               (range (count +hand-order+)))))
               

(defn which-hand [h]
  (some #(let [f (get +hand-pred+ %1)]
           (and (f h) %1))
        +hand-order+))

(defn describe-hand [h]
  (which-hand (handinfo h)))

(defn beats [hand-type-a hand-type-b]
  "Determine if hand-type A beats hand-type B"
  (hand-type-b (get +hand-beats+ hand-type-a)))