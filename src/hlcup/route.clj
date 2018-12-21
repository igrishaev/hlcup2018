(ns hlcup.route
  (:require
   [bidi.bidi    :as bidi]))


(def router
  ["/accounts/filter/" {:get :hlcup.api-filter/handler}
   "/accounts/group/"  {:get :hlcup.api-group/handler}

   true :default])


(defmulti dispatch :handler)


(defmethod dispatch :default
  [request]
  {:status 404
   :body nil})


(defn wrap-route
  [handler]
  (fn [{:keys [uri] :as request}]
    (handler (bidi/match-route* router uri request))))
