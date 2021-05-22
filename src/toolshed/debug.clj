(ns toolshed.debug)

;; Copy this into your namespace requires:
;; [toolshed.debug :refer [locals tap-> ?> ?->]]

;; Adapted from shadow.debug
;; https://github.com/thheller/shadow-cljs/

(defn dbg-info [form]
  (let [{:keys [line column]} (meta form)]
    (-> {:ns (str *ns*)}
        (cond->
         line
          (assoc :line line)
          column
          (assoc :column column)))))

(defmacro locals []
  (reduce-kv
   (fn [m local info]
     (assoc m (keyword (name local)) local))
   {}
   (if (:ns &env)
     (:locals &env)
     &env)))

(defn tap-> [obj]
  (tap> obj)
  obj)

(defmacro ?>
  [obj]
  `(tap> (merge {:? ~obj} {:where ~(dbg-info &form)}
                (locals)))

(defmacro ?->
  [obj]
  `(do (tap> (merge {:? ~obj} {:where ~(dbg-info &form)}
                    (locals)))
       ~obj))