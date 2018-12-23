(ns hlcup.app
  (:require

   hlcup.api.filter
   hlcup.api.group

   [hlcup.error :as error]

   [compojure.core :refer [defroutes GET POST]]

   [ring.middleware.json
    :refer [wrap-json-response]]

   [ring.middleware.keyword-params
    :refer [wrap-keyword-params]]

   [ring.middleware.params
    :refer [wrap-params]]))


(defroutes app-naked

  (GET "/accounts/filter"
       request (hlcup.api.filter/handler request))

  (GET "/accounts/group"
       request (hlcup.api.group/handler request))

  (fn [_]
    {:status 404
     :body "not found"}))


(def app
  (-> app-naked
      wrap-keyword-params
      wrap-params
      wrap-json-response
      error/wrap-exception))
