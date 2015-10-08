(ns wonderland-number.finder)

(def num-digits 6)
(def min-range (int (Math/pow 10 (dec num-digits))))
(def max-range (int (Math/pow 10 num-digits)))

;; (defn- digits [n]
;;   (loop [current n
;;          res #{}]
;;     (if (< current 10)
;;       (conj res current)
;;       (recur (quot current 10) (conj res (mod current 10))))))
(def digits (comp set str))

(def factors (range 1 7))

(defn- product-fns []
  (for [factor factors]
    (partial * factor)))

(defn wonderland-number []
  (->> (range min-range max-range)
       (map #((apply juxt (product-fns)) %))
       (filter #(->> %
                     (map digits)
                     (apply =)))
       (first)
       (first)))
