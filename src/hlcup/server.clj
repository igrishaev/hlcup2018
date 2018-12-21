(ns hlcup.server
  (:import java.io.Closeable)

  (:require

   [hlcup.route :as route]

   [aleph.http  :as http]
   [aleph.netty :as netty]

   [mount.core :as mount]))


(declare server)


(defn server-start
  []

  (http/start-server

   (-> route/dispatch
       route/wrap-route)

   {:port 8088}))


(defn server-stop
  []
  (.close ^Closeable server)
  (netty/wait-for-close server))


(mount/defstate
  ^{:on-reload :noop}
  server
  :start (server-start)
  :stop (server-stop))


(defn start
  []
  (mount/start #'server))


(defn stop
  []
  (mount/stop #'server))
