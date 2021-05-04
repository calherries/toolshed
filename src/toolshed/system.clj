(ns toolshed.system
  (:require [clojure.tools.namespace.repl :as tn-repl]
            [ring.adapter.jetty9 :as jetty]
            [reitit.ring :as reitit]))

;; This is a map that maintains the state of the system.
(defonce system (atom nil))

(defn refresh []
  (let [{:keys [after-refresh stop]} @system]
    (doseq [f stop]
      (f))
    (tn-repl/refresh :after after-refresh)))

(defn start-system [config components]
  (reset! system (merge {:stop '()} config))
  (run! #(reset! system (% @system)) components))

(defn stop-system []
  (run! #(%) (:stop @system))
  (reset! system (atom nil)))

(defn jetty [{:web/keys [port handler host]
              :or {port 8080
                   host "0.0.0.0"}
              :as sys}]
  (let [server (jetty/run-jetty handler {:port port
                                         :host host
                                         :join? false})]
    (println "Jetty running on" (str "http://localhost:" port))
    (update sys :stop conj #(jetty/stop-server server))))

(defn hello-world-handler [req]
  {:status 200
   :body "Hello, world!"})

(defn hello-world-component [sys]
  (assoc sys :web/handler hello-world-handler))

;; Components is a list of functions that change the state map. This
(def hello-world-components
  [hello-world-component
   jetty])

(def hello-world-routes
  ["/" {:get {:handler hello-world-handler}}])

(defn hello-world-routes-component [sys]
  (assoc sys :web/routes hello-world-routes))

(defn reitit [{:keys [web/routes] :as sys}]
  (let [router (reitit/router routes)]
    (assoc
     sys
     :web/router router
     :web/handler (reitit/ring-handler router))))

(comment
  (reitit/router hello-world-routes))

(def hello-world-reitit-components
  [hello-world-routes-component
   reitit
   jetty])

(comment
  (/ 1 0)
  (keys (ns-publics 'ring.adapter.jetty9))
  (start-system {:first-start true} hello-world-components)
  (start-system {:first-start true} hello-world-reitit-components)
  @system
  (stop-system))
