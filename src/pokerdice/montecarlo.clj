(ns pokerdice.montecarlo
  "Functions for creating random hands and performing simulations"
  (:use clojure.core
        pokerdice.constants
        pokerdice.util
        [pokerdice.hands :only (which-hand describe-hand)]))

(def *rollstream*
  (repeatedly #(inc (rand-int +maxface+))))
(def *handstream*
  (partition 5 *rollstream*))

(defn normalize-frequencies [freqs]
  (let [tot (reduce + (vals freqs))]
    (zipmap (keys freqs)
            (map #(/ %1 tot) (vals freqs)))))
(defn general-freqs [n]
  (normalize-frequencies (frequencies (take n (map describe-hand *handstream*)))))

(defn report-on-freqs [freqs]
  (let [ordered-keys (sort #(compare (%2 freqs) (%1 freqs)) (keys freqs))]
    (tableify (map (fn [k]
                     (list (str k) (format "%.2f" (float (* 100 (get freqs k))))))
                   ordered-keys)
              '("Hand" "%")
              " | ")))

