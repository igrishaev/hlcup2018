(ns hlcup.api.likes
  (:require
   [hlcup.spec]
   [hlcup.error :as error]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]

   [clojure.set :as set]))


(def into-map (partial into {}))

(defn _exists?
  [field value]
  (some? (:db/id (db/pull [:db/id] [field value]))))

(def exists-id? (partial _exists? :account/id))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [likes]} params

        ids (transient [])
        _ (doseq [like likes
                  :let [{:keys [liker likee ts]} like]]
            (conj! ids liker)
            (conj! ids likee))

        ids (persistent! ids)
        _ (when (some false? (map exists-id? ids))
            (error/error 400 ""))

        trx
        (for [[liker likes]
              (group-by :liker likes)]

          {:account/id liker
           :account/likes (for [like likes
                                :let [{:keys [likee ts]} like]]
                            {:like/id likee
                             :like/ts ts})})]

    ;; (clojure.pprint/pprint trx)

    (db/transact trx)

    {:status 202
     :body "{}"}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
