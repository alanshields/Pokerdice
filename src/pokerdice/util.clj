(ns pokerdice.util
  (:use clojure.core))

(defn tableify [coll headers sep]
  "Turns collection of lists of strings into collection of strings"
  (let [all-data (if headers
                   (cons headers coll)
                   coll)]
    (let [numcols (apply max (map count all-data))
          col-widths (map (fn [col]
                            (let [elements (map #(nth %1 col "") all-data)]
                              (apply max (map count elements))))
                          (range numcols))]
      (letfn [(format-string-to-width [astring width]
                                      (format (str "%" width "s") astring))
              (format-line [line-coll]
                           (apply str
                                  (interpose sep
                                             (map (fn [column-number]
                                                    (format-string-to-width (nth line-coll column-number "") (nth col-widths column-number)))
                                                          (range numcols)))))]
        (let [formatted-data (map format-line coll)
              formatted-headers (when headers
                                  (format-line headers))
              with-headers (if headers
                             (cons formatted-headers
                                   (cons (apply str (repeat (count formatted-headers) "-"))
                                         formatted-data))
                             formatted-data)]
          (apply str (interpose "\n" with-headers)))))))
          