(ns hlcup.api.update
  (:require
   [hlcup.spec]
   [hlcup.error :as error]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]

   [clojure.set :as set]))


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


        {:keys [id email premium interests]} params

        account (db/pull [:db/id
                          :account/interests
                          :account/email]
                         [:account/id id])

        a (:db/id account)

        _ (when-not a
            (error/error 404 ""))

        _ (when (and
                 email
                 (not= email (:account/email account))
                 (exists-email? email))

            (error/error 400 ""))

        _interests (-> account :account/interests seq)

        params (dissoc params :query_id)

        account (add-ns params "account")
        account (cond-> account

                  premium
                  (update :account/premium
                          add-ns
                          "premium"))



        trx [account]
        trx (if (and interests _interests)
              (let [delete (set/difference
                            (set _interests)
                            (set interests))]
                (concat
                 (for [i delete]
                   [:db/retract a :account/interests i])
                 trx)))]

    (db/transact trx)

    {:status 202
     :body "{}"}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
