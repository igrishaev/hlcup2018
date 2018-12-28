(ns hlcup.load

  (:import (java.util.zip
            ZipFile
            ZipEntry))

  (:require
   [clojure.string :as str]

   [cheshire.core :as json]
   [clojure.java.io :as io]))


(defn read-zip
  [^String path]

  (let [zip (ZipFile. path)
        entries (-> zip .entries enumeration-seq)]

    (for [^ZipEntry e entries
          :let [filename (.getName e)]
          :when (str/ends-with? filename ".json")]

      (.getInputStream zip e))))


(defn read-stream
  [stream]
  (json/parse-stream (io/reader stream) true))


(defn load-zip
  [path]
  (for [stream (read-zip path)
          :let [data (read-stream stream)]]
    (:accounts data)))


(defn ->sex
  [value]
  (case value
    "m" :sex/m
    "f" :sex/f))


(defn ->status
  [value]
  (case value
    "свободны"   :status/free
    "заняты"     :status/busy
    "всё сложно" :status/complex))


(defn ->account
  [model]

  (let [{:keys [id
                email
                fname
                sname
                phone
                sex
                birth
                country
                city
                joined
                status
                interests
                premium
                likes]} model

        {:keys [start
                finish]} premium

        premium (cond-> nil

                  start
                  (assoc :premium/start start)

                  finish
                  (assoc :premium/finish finish))

        likes (for [like likes
                    :let [{:keys [id ts]} like]]

                {:like/id {:account/id id}
                 :like/ts ts})

        account {:account/id     id
                 :account/email  email
                 :account/sex    (->sex sex)
                 :account/birth  birth
                 :account/joined joined
                 :account/status (->status status)
                 :account/likes  likes}]

    (cond-> account

      premium
      (assoc :account/premium premium)

      interests
      (assoc :account/interests interests)

      fname
      (assoc :account/fname fname)

      sname
      (assoc :account/sname sname)

      phone
      (assoc :account/phone phone)

      country
      (assoc :account/country country)

      city
      (assoc :account/city city))))


(def ->accounts
  (partial map ->account))


(defn read-all
  []
  (->> "/Users/ivan/Downloads/test_accounts_261218/data/data.zip"
       load-zip
       (map ->accounts)))
