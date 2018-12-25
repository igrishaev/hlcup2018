(ns hlcup.db
  (:import datomic.peer.LocalConnection)

  (:require
   hlcup.db.func

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


(def uri "datomic:mem://hlcup7")



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


(defn pull
  ([ref]
   (pull '[*] ref))
  ([pattern ref]
   (d/pull (d/db conn) pattern ref)))


(defn query
  [query args]
  (d/query
   {:query query
    :args (cons (d/db conn) args)}))

;; tr

(declare tr)

(defn read-tr
  [& _]

  {(-> :sex/f pull :db/id)
   "f"

   (-> :sex/m pull :db/id)
   "m"

   :sex/f "f"
   :sex/m "m"

   (-> :status/free pull :db/id)
   "свободны"

   (-> :status/busy pull :db/id)
   "заняты"

   (-> :status/complex pull :db/id)
   "всё сложно"

   :status/free
   "свободны"

   :status/busy
   "заняты"

   :status/complex
   "всё сложно"})


(defn set-tr
  []
  (alter-var-root #'tr read-tr))


;; todo
(def NOW 1545699626)
