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
  (with-open [reader (io/reader "/Users/ivan/Downloads/data/test_accounts_251218/answers/phase_1_get.answ")]
    (doall

     (take
      99999
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


(defn chunks
  [n coll]
  (partition n n [] coll))


(defn read-post1
  []
  (with-open [reader (io/reader "/Users/ivan/Downloads/data/test_accounts_251218/ammo/phase_2_post.ammo")]

    (doall
     (loop [lines (take 99999999 (line-seq reader))
            result []]

       (if (empty? lines)
         result

         (let [[block rest] (split-with #(not= "" %) lines)

               [_ request] block
               [_ json & rest] rest
               [method uri _] (str/split request #" ")

               node [(-> method str/lower-case keyword)
                     (format "http://127.0.0.1:8088%s" uri)
                     (json/parse-string json true)]

               result (conj result node)]

           (recur rest result)))))))


(defn read-post2
  []

  (with-open [reader (io/reader "/Users/ivan/Downloads/data/test_accounts_251218/answers/phase_2_post.answ")]
    (let [rows (csv/read-csv reader :separator \tab)]
      (doall
       (for [[_method _uri status body] rows]
         [(Integer/parseInt status)
          (json/parse-string body true)])))))


(defn read-post3
  []
  (map
   (fn [[method url payload] [status body]]
     [method url payload status body])
   (read-post1)
   (read-post2)))


#_
"inzodbamebteuw@yahoo.com"

(def page 6)
(def step 50)


(deftest test-post

  (doseq [row (->> (read-post3)
                   (drop 0)
                   (take 99999)

                   #_
                   (drop (* page step))
                   #_
                   (take step))]

    (let [[method uri payload status body] row]

      (when (re-find #"/accounts/\d+" uri)

        (let [response (client/request {:method method
                                        :url uri
                                        :form-params payload
                                        :content-type :json
                                        :throw-exceptions false
                                        :as :json})]


          (testing (str uri " " payload)

            (is (= status (:status response)))
            (is (= (or body "") (or (:body response) ""))))))

      )

    )
  )


#_
(deftest test-post
  (doseq [row (->> _post

                   #_
                   (drop (* page step))
                   #_
                   (take step))]

    (let [[method uri payload status body] row]

      (when (re-find #"/accounts/new/" uri)

        (let [response (client/request {:method method
                                        :url uri
                                        :form-params payload
                                        :content-type :json
                                        :throw-exceptions false
                                        :as :json})]


          (testing (str uri " " payload)

            (is (= status (:status response)))
            (is (= body (:body response))))))

      )

    )
  )

(deftest test-data
  []

  (doseq [row (transform)]

    (let [[method uri status body] row]

      (when true #_(re-find #"/suggest/" uri)

            (let [url (format "http://127.0.0.1:8088%s" uri)
                  response (client/request {:method method
                                            :url url
                                            :throw-exceptions false
                                            :as :json})]


              (testing url

                (is (= status (:status response)))
                (is (= body (:body response))))))

      #_
      (when-not
          (or (= status (:status response))
              (= body (:body response)))
          (throw (ex-info "stop" {})))

      )

    )


  )
