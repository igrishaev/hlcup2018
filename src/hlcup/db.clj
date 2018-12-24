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


(def uri "datomic:mem://hlcup6")


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
  [pattern ref]
  (d/pull (d/db conn) pattern ref))


(defn query
  [query args]
  (d/query
   {:query query
    :args (cons (d/db conn) args)}))


;;;;;;;;


(defn bar
  []

  (d/q '[:find [?target ...]
         :in $ ?a
         :where
         [?a :account/likes ?like]
         [?like :like/id ?target]]
       (d/db conn)
       [:account/id 42]))


(defn xxx
  [accounts timestamps]

  (println accounts)

  #_
  (zipmap accounts timestamps))

#_
(defn baz
  []

  (d/q '[:find ?a ?target ?ts ?diff
         :in $ [?target ...] ?sex ?mapping
         :where
         [?like :like/id ?target]

         [?a :account/likes ?like]
         [?a :account/sex ?sex]

         [?like :like/ts ?ts]

         [(clojure.core/get ?mapping ?a -1) ?diff]

         ]
       (d/db conn)
       _targets
       :sex/f
       {}

       ))


(defn foo []

  (d/query {:query '{:find

                     [?id
                      ?status
                      (count ?interest)
                      ?diff
                      ?email
                      ?fname
                      ?sname
                      ?birth

                      ?city

                      ]

                     :in [$ ?_sex [?_interest ...] ?_birth ?city]
                     :where
                     [


                      [?a :account/interests ?_interest]
                      [?a :account/sex ?_sex]



                      ;; [?a :account/country ?country]

                      [?a :account/id     ?id]
                      [?a :account/email  ?email]
                      [?a :account/status ?status]
                      [?a :account/fname  ?fname]
                      [?a :account/sname  ?sname]
                      [?a :account/birth  ?birth]

                      [(hlcup.db.func/dist ?birth ?_birth) ?diff]

                      ]

                     }
            :args [(d/db conn) :sex/m ["50 Cent" "Целоваться"] 444444 "Ньюгорск"]}))
