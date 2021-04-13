(ns toolshed.core
  "A small collection of useful functions and macros.")

(defn map-values [f m]
  "Map the values of a hash-map"
  (reduce-kv (fn [acc k v]
               (assoc acc k (f v)))
             {}
             m))

(defn map-keys [f m]
  "Map the values of a hash-map"
  (reduce-kv (fn [acc k v]
               (assoc acc (f k) v))
             {}
             m))