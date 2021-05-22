(ns toolshed.debug
  (:require-macros [toolshed.debug]))

;; Copy this into your namespace requires:
;; [toolshed.debug :refer [locals tap-> ?> ?->]]

(defn tap-> [obj]
  (tap> obj)
  obj)