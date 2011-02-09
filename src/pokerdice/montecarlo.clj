(ns pokerdice.montecarlo
  "Functions for creating random hands and performing simulations"
  (:use clojure.core
        pokerdice.constants
        pokerdice.util
        [pokerdice.hands :only (which-hand describe-hand +hand-beats+)]
        [clojure.contrib.combinatorics :only (subsets)]
        [clojure.contrib.greatest-least :only (greatest greatest-by)]))

(def *rollstream*
  (repeatedly #(inc (rand-int +maxface+))))
(def *handstream*
  (partition +hand-size+ *rollstream*))

(def +randsize+ 2000)

(defn normalize-frequencies [freqs]
  (let [tot (reduce + (vals freqs))]
    (zipmap (keys freqs)
            (map #(/ %1 tot) (vals freqs)))))


(defn report-on-freqs [freqs]
  (let [ordered-keys (sort #(compare (%2 freqs) (%1 freqs)) (keys freqs))]
    (tableify (map (fn [k]
                     (list (str k) (format "%.2f" (float (* 100 (get freqs k))))))
                   ordered-keys)
              '("Hand" "%")
              " | ")))

;;; Definitions:
;; Hand: '(1 5 5 6 2) list of five rolls
;; Keeps: '(1 2 6) sub-list of five or less rolls
;; hand-type-distribution: '(pair pair pair two-pair two-pair ...) hand-types for simulated distribution
;; Freqs: histogram of hand types

(defn distribution->freqs [dist]
  (normalize-frequencies (frequencies (map describe-hand dist))))

(defn calculate-general-freqs [n]
  (distribution->freqs (take n *handstream*)))
;;; calculated using calculate-general-freqs with a size of 200,000
(def +free-draw-freqs+
  {'five-of-a-kind 359/500000, 'straight 15393/500000, 'three-of-a-kind 76891/500000, 'four-of-a-kind 9727/500000, 'nothing 3107/50000, 'pair 4633/10000, 'two-pair 23081/100000, 'full-house 3901/100000})
(defn calc-hand-type-score-by-beating-freq [hand-type]
  (reduce + (map +free-draw-freqs+ (hand-type +hand-beats+))))
(def +hand-type-scores-by-beating-freq+
  (zipmap +hand-order+ (map calc-hand-type-score-by-beating-freq +hand-order+)))

(defn hand-type-value-by-beating-freq [hand-type]
  (+hand-type-scores-by-beating-freq+ hand-type))

(defn mean [& coll]
  (/ (reduce + coll) (count coll)))
(defn mev-score-for-dist [dist]
  (apply mean (map hand-type-value-by-beating-freq (map describe-hand dist))))
(defn better-distribution-by-expected-value [dist-a dist-b]
  (> (mev-score-for-dist dist-a) (mev-score-for-dist dist-b)))


(defn dist-for-keeps [keeps]
  "Given a sub-hand KEEPS, run monte carlo of possible completions"
  (if (= +hand-size+ (count keeps))
    (list keeps)
    (take +randsize+
          (map (fn [completion] (concat keeps completion))
               (partition (- +hand-size+ (count keeps)) *rollstream*)))))

(defn report-on-freqs-with-mev [freqs]
  (let [ordered-keys (sort #(compare (%2 freqs) (%1 freqs)) (keys freqs))]
    (tableify (map (fn [k]
                     (list (str k)
                           (format "%.2f" (float (* 100 (get freqs k))))
                           (format "%.2f" (float (hand-type-value-by-beating-freq k)))
                           (format "%.2f" (float (* (get freqs k) (hand-type-value-by-beating-freq k))))))
                   ordered-keys)
              '("Hand" "%" "V" "MEV")
              " | ")))

(defn println-describe-keep
  ([keeps dist]
     (println "Keep:" keeps)
     (println (report-on-freqs-with-mev (distribution->freqs dist)))
     (println "MEV:" (format "%.2f" (float (mev-score-for-dist dist)))))
  ([keeps]
     (println-describe-keep keeps (dist-for-keeps keeps))))

(defn best-keep-and-resulting-freqs [hand]
  "Given hand HAND, determine best keep.

Returns (list KEEP freq)"
  (let [all-possible-keeps (cons '()
                                 (filter #(< 1 (count %1)) (subsets hand)))
        keep-to-distribution (zipmap all-possible-keeps (map dist-for-keeps all-possible-keeps))]
    (let [winner (apply greatest-by (cons #(mev-score-for-dist (keep-to-distribution %1))
                                          all-possible-keeps))]
      (list winner (keep-to-distribution winner)))))

(defn println-analyze-hand [hand]
  (let [[keeps dist] (best-keep-and-resulting-freqs hand)]
    (println-describe-keep keeps dist)))


(defn myag [my-hand opp-hand]
  (println "Me:")
  (println-analyze-hand my-hand)
  (println "\n\nOpponent:")
  (println-analyze-hand opp-hand))

(defn z [& rolls]
  (if (not (= 10 (count rolls)))
    (println "Try again. Not enough rolls.")
    (myag (take +hand-size+ rolls) (nthnext rolls 5))))