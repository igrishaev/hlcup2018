(ns hlcup.error)


(defn error
  [status template & args]
  (throw (ex-info
          (apply format template args)
          {:status status})))


(defn exception->response
  [^Exception e]
  (let [data (ex-data e)
        {:keys [status]} data
        message (.getMessage e)]
    {:status (or status 500)
     :body message}))


(defn wrap-exception
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (exception->response e)))))
