(ns hlcup.api.new
  (:require
   [hlcup.spec]
   [hlcup.error :as error]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


(def into-map (partial into {}))


(defn add-ns
  [_map _ns]
  (into-map
   (for [[k v] _map]
     [(keyword _ns (name k)) v])))


(defn _exists?
  [field value]
  (some? (:db/id (db/pull [:db/id] [field value]))))

(def exists-id? (partial _exists? :account/id))
(def exists-email? (partial _exists? :account/email))

;; todo validate email

(defn ->likes
  [likes]
  (for [like likes]
    (add-ns like "like")))


(defn _handler
  [request]


  (let [{:keys [params]} request

        {:keys [id email likes premium]} params

        _ (when (exists-id? id)
            (error/error 404 "id %s exists" id))

        _ (when (exists-email? email)
            (error/error 404 "email %s exists" email))

        ;; todo check nil?
        _ (when (some nil?
                      (map exists-id?
                           (map :id likes)))
            (error/error 404 ""))

        params (dissoc params :query_id)

        account (add-ns params "account")
        account (cond-> account

                  premium
                  (update :account/premium
                          add-ns
                          "premium")

                  likes
                  (update :account/likes ->likes)
                  )]

    (db/transact [account])

    {:status 201
     :body "{}"}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
