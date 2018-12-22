(ns hlcup.server
  (:import java.io.Closeable)

  (:require
   [hlcup.app :refer [app]]

   [aleph.http  :as http]
   [aleph.netty :as netty]
   [mount.core :as mount]))


(declare server)


(defn server-start
  []
  (http/start-server
   #'app
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
