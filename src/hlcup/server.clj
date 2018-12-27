(ns hlcup.server
  (:import java.io.Closeable)

  (:require
   [hlcup.app :refer [app]]

   [aleph.http  :as http]
   [aleph.netty :as netty]
   [mount.core :as mount]))


(declare server)


(defmacro with-sleep
  [timeout & body]
  `(let [result# (do ~@body)]
     (Thread/sleep ~timeout)
     result#))


(defn server-start
  []
  (with-sleep 3000
    (http/start-server
     #'app
     {:port 8088})))


(defn server-stop
  []
  (with-sleep 3000
    (.close ^Closeable server)
    (netty/wait-for-close server)))


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


(comment
  (start)
  server
  (stop))
