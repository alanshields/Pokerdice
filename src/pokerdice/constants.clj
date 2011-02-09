(ns pokerdice.constants
  (:use clojure.core))

(def +maxface+ 6)
(def +hand-order+
  '(five-of-a-kind four-of-a-kind full-house straight three-of-a-kind two-pair pair nothing))
;; if you change hand size, check straight? in hands.clj
(def +hand-size+ 5)
