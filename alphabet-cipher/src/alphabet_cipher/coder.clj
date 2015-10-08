(ns alphabet-cipher.coder
  (:require [clojure.set :refer [map-invert]]))

(def ascii-offset (int \a))
(def charset (map char (range (int \a) (inc (int \z)))))
(def charset-size (count charset))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Efficient solution ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn- keyword-char [keyword pos]
;;   (let [max-len (.length keyword)
;;         keyword-pos (mod pos max-len)]
;;     (nth keyword keyword-pos)))

;; (defn- encode-char [row-letter column-letter]
;;   (let [row-offset (- (int row-letter) ascii-offset)
;;         column-num (- (int column-letter) ascii-offset)]
;;     (char (+ ascii-offset (mod (+ row-offset column-num) charset-size)))))

;; (defn encode [keyword message]
;;   (apply str (map (fn [c pos]
;;                     (encode-char c (keyword-char keyword pos)))
;;                   message
;;                   (range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building substitution-chart ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- offset-char [offset c]
  (let [pos (- (int c) ascii-offset)
        new-pos (mod (+ pos offset) charset-size)]
    (char (+ new-pos  ascii-offset))))

(defn- substitution-chart-row [c]
  (let [offset (- (int c) ascii-offset)]
    (zipmap charset
            (map (partial offset-char offset) charset))))

(def substitution-chart (zipmap charset (map substitution-chart-row charset)))

(defn- encode-char [row-letter column-letter]
  (get-in substitution-chart [row-letter column-letter]))

(defn encode [keyword message]
  (apply str (map encode-char message (apply concat (repeat keyword)))))

;; (encode "scones" "meetmebythetree")

(defn- decode-char [message-letter row-letter]
  (-> (get substitution-chart row-letter)
      (map-invert)
      (get message-letter)))


(defn decode [keyword message]
  (apply str (map decode-char message (apply concat (repeat keyword)))))

;; (decode "scones" "egsgqwtahuiljgs")

(defn- find-min-keyword [s]
  (let [max-len (count s)]
    (loop [n 1]
      (if (= n max-len)
        s
        (let [candidate (subs s 0 n)
              pattern (re-pattern (format "^(?:%s)+(.*)$" candidate))
              remain (second (re-find pattern s))]
          (if (and remain (.startsWith s remain))
            candidate
            (recur (inc n))))))))

;; (find-min-keyword "sconessconessco")

(defn decypher [cypher message]
  (-> (decode message cypher)
      (find-min-keyword)))

;; (decypher "egsgqwtahuiljgs" "meetmebythetree")

(def decipher decypher)

