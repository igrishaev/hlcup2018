(ns hlcup.db
  (:import datomic.peer.LocalConnection)

  (:require
   [hlcup.load :as load]

   [datomic.api :as d]
   [mount.core :as mount]

   [clojure.java.io :as io]))


(declare conn)


(defn read-edn
  [filename]
  (-> filename
      io/resource
      slurp
      read-string))


(defn load-schema
  [_conn]
  (d/transact _conn (read-edn "schema.edn")))


(def uri "datomic:mem://hlcup5")


(defn db-init
  []
  (d/create-database uri)
  (let [_conn (d/connect uri)]
    @(load-schema _conn)
    @(d/transact _conn (load/read-all))
    _conn))


(defn db-close
  []
  (.release ^LocalConnection conn)
  (d/delete-database uri)
  nil)


(mount/defstate
  ^{:on-reload :noop}
  conn
  :start (db-init)
  :stop (db-close))


(defn start
  []
  (mount/start #'conn))


(defn stop
  []
  (mount/stop #'conn))


(defn query
  [params]
  (d/query (update params :args cons (d/db conn))))
