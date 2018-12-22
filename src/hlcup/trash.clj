(defn foo
  []

  (d/q

   '[:find

     ?target-id
     ?other-id

     (sum ?diff)

     ;; (max ?account-ts)
     ;; (avg ?other-ts)

     :in $ ?account

     :with
     ?account-ts
     ?other-ts

     :where

     [?account        :account/likes  ?account-like]
     [?account-like   :like/id        ?account-target]
     [?account-like   :like/ts        ?account-ts]

     [?other-like     :like/id        ?account-target]
     [?account-target :account/id     ?target-id]

     [?other-like     :like/ts        ?other-ts]
     [?other-account  :account/likes  ?other-like]
     [?other-account  :account/id     ?other-id]

     [(hlcup.db/ggg ?account-ts ?other-ts) ?diff]


     ]

   (d/db conn)
   [:account/id 1]
   ))


(defn bar
  []

  (d/q

   '[:find

     ?sex
     ?country
     (count ?account)

     :in
     $
     ;; ?account

     :where

     [?account        :account/sex      ?sex]
     [(get-else $ ?account :account/country "") ?country]

     ]

   (d/db conn)
   ;; [:account/id 4066]

   ))

(defn baz
  []

  (d/q

   '[:find ?id
     :in $
     :where
     [?account        :account/id      ?id]

     not null
     [?account :account/country _]

     null
     [(missing? $ ?account :account/country)]

     ]

   (d/db conn)

   ))


(defn ccc
  [values]

  (reduce
   (fn [result value]
     (if value
       (inc result)
       result))
   0
   values))


(defn aaa
  []

  (d/q

   '[:find

     ?id

     ;; (distinct ?interest)
     ;; (distinct ?other-interest)

     ?other

     (hlcup.db/ccc ?match)

     :in
     $
     ?account
     ?country

     :where

     [?account        :account/id          ?id]
     [?account        :account/interests   ?interest]

     [?account        :account/birth       ?birth]

     [?other          :account/sex         :sex/m]
     ;; [?other          :account/country     ?country]

     [?other          :account/interests   ?other-interest]
     [?other          :account/birth       ?other-birth]

     [(= ?other-interest ?interest) ?match]


     ]

   (d/db conn)
   [:account/id 1]
   "Гератрис"
   ))


(defn a
  []
  (d/q

   '[:find ?id
     :in $
     :where

     [?a :account/interests "50 Cent"]
     [?a :account/interests "Целоваться"]
     [?a :account/id ?id]]

   (d/db conn)))


(defn b
  []
  (d/q

   '[:find ?id
     :in $ ?iii
     :where

     [(datomic.api/entity $ ?a) ?e]
     [(:account/interests ?e) ?interests]
     [(clojure.set/superset? ?interests ?iii)]
     [?a :account/id ?id]]

   (d/db conn)
   #{"50 Cent" "Целоваться"}


   ))


(defn c
  []
  (d/q

   '[:find ?id
     :in $ ?iii
     :where
     [?a :account/id ?id]
     [(datomic.api/entity $ ?a) ?e]
     [(:account/interests ?e) ?interests]
     [(hlcup.db/superset? ?interests ?iii)]]

   (d/db conn)
   #{"50 Cent" "Выходные" "Кинокифильмы"}


   ))


(defn d
  []
  (d/q

   '[:find (count ?id)
     :in $
     :where
     [?a :account/id ?id]]

   (d/db conn)))


(defn e
  []

  (d/q

   '[:find ?id
     :in $ [?interest ...]
     :where
     [?a :account/id ?id]
     (not-join [?a ?interest]
               [?a :account/interests ?i]
               (not [(= ?i ?interest)]))]

   (d/db conn)
   #{"50 Cent" "Выходные" "Кинокифильмы" "Солнце" "Целоваться"}

   ))



(ns hlcup.middleware

  (:require
   [hlcup.error :as e]

   [manifold.deferred  :as d]
   [cheshire.core      :as json]
   [clojure.walk       :refer [keywordize-keys]]

   [clojure.spec.alpha :as s]))


(defn wrap-keywordize
  "Transform headers and params into keywords"
  [handler]
  (fn [request]
    (d/chain
     (update request :params keywordize-keys)
     handler)))


(defn jsonify
  [body]
  (-> body
      json/generate-string
      (.getBytes "utf-8")))


(defn wrap-json-response
  "Turns a data structure response into a JSON-encoded one."
  [handler]
  (fn [request]

    (d/chain
     request
     handler

     (fn [response]

       (if (-> response :body coll?)

         (-> response
             (update :body jsonify)
             (assoc-in [:headers :content-type]
                       "application/json; charset=utf-8"))

         response)))))


(def invalid :clojure.spec.alpha/invalid)


(defn validate-request
  [request]

  (let [{:keys [params handler]} request
        spec (s/get-spec handler)]

    (if spec

      (let [result (s/conform spec params)
            valid? (not= result invalid)]

        (if valid?
          (assoc request :params result)
          (e/error 400 "wrong input params")))

      request)))


(defn wrap-spec-in
  "
  Validates the incoming request with a spec.
  We try to take a spec that `:handler` field
  is referencing to.
  "
  [handler]
  (fn [request]
    (d/chain
     request
     validate-request
     handler)))



(ns hlcup.error
  (:require
   [manifold.deferred :as d]))


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
  "Top-level wrapper that handles and reports any errors."
  [handler]
  (fn [request]

    (-> request

        (d/chain handler)

        (d/catch
            (fn [^Exception e]
              (exception->response e))))))
