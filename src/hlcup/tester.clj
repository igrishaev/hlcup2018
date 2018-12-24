(ns hlcup.tester
  (:require [clj-http.client :as client]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]

            [clojure.test :refer :all]
            [hlcup.app :refer [app]]))


(defn load-data
  []
  (with-open [reader (io/reader "/Users/ivan/Downloads/phase_1_get.answ")]
    (doall

     (take
      18
      (csv/read-csv reader :separator \tab))))

  )


(defn transform
  []
  (for [[method uri status body] (load-data)]

    [(-> method name str/lower-case keyword)
     uri
     (Integer/parseInt status)
     (or
      (json/parse-string body true)
      "")]))


(deftest test-data
  []

  (doseq [row (transform)]

    (let [[method uri status body] row
          url (format "http://127.0.0.1:8088%s" uri)
          response (client/request {:method method
                                    :url url
                                    :throw-exceptions false
                                    :as :json})]

      (testing url

        (is (= status (:status response)))
        (is (= body (:body response))))

      (when-not
          (or (= status (:status response))
              (= body (:body response)))
        (throw (ex-info "stop" {})))

      )

    )


  )
