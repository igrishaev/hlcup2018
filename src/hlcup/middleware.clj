(ns hlcup.middleware

  (:require
   [hlcup.error :as e]

   [clojure.spec.alpha :as s]))


(def invalid :clojure.spec.alpha/invalid)


(defn wrap-spec
  [handler spec]
  (fn [request]

    (let [{:keys [params]} request]

      (let [result (s/conform spec params)
            valid? (not= result invalid)]

        (if valid?
          (handler (assoc request :params result))

          ;; todo drop report
          (let [report (s/explain-str spec params)]
            (e/error 400 ""))

          #_
          (e/error 400 "wrong input params"))))))
