(ns hlcup.api.recommend
  (:require
   [hlcup.spec]
   [hlcup.error :as error]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


(def scope-base
  '{:find []
    :in [$]
    :where []
    :args []
    :fields []})


(defmacro push
  [vector key & values]
  `(update ~vector ~key conj ~@values))


(defn apply-defaults
  [scope]
  (-> scope
      (push :where
            '[?a :account/id     ?id]
            '[?a :account/status ?status]
            '[?a :account/birth ?birth]
            '[?a :account/email  ?email]
            '[(get-else $ ?a :account/fname "N/A") ?fname]
            '[(get-else $ ?a :account/sname "N/A") ?sname])

      (push :find
            '?id
            '?status
            '(count ?interests)
            '?birth
            '(pull ?a [:account/premium])
            '?email
            '?fname
            '?sname
            )))


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))


(defmethod apply-predicate
  :interests
  [scope _ value]
  (-> scope
      (push :where  '[?a :account/interests ?interests])
      (push :in     '[?interests ...])
      (push :args   value)))


(defmethod apply-predicate
  :sex
  [scope _ value]
  (-> scope
      (push :where  '[?a :account/sex ?sex])
      (push :in     '?sex)
      (push :args   value)))


(defmethod apply-predicate
  :country
  [scope _ value]
  (-> scope
      (push :where  '[?a :account/country ?country])
      (push :in     '?country)
      (push :args   value)))


(defmethod apply-predicate
  :city
  [scope _ value]
  (-> scope
      (push :where  '[?a :account/city ?city])
      (push :in     '?city)
      (push :args   value)))


(defn params->scope
  [params]
  (let [{:keys [interests
                sex
                birth
                country
                city]} params]

    (cond-> scope-base

      true
      (->
       (apply-predicate :interests interests)
       (apply-predicate :sex sex))

      city
      (apply-predicate :city city)

      country
      (apply-predicate :country country)

      true
      (apply-defaults))))



(defn status->points
  [status]
  (case  (-> status db/tr)
    "свободны" 2
    "всё сложно" 1
    "заняты" 0))


(defn is-premium?
  [premium]
  (and
   premium
   (<= (-> premium :account/premium :premium/start)
       db/NOW
       (-> premium :account/premium :premium/finish))))


(defn row-sorter
  [_birth]
  (fn [row]
    (let [[id
           status
           interests
           birth
           premium] row]

      ;; (println "--------")
      ;; (println (is-premium? premium) premium)
      ;; (println (db/tr status) (status->points status))
      ;; (println (- interests))
      ;; (println (Math/abs ^long (- _birth birth)))

      [(is-premium? premium)
       (status->points status)
       (- interests)
       (Math/abs ^long (- _birth birth))
       id])))


(defn row->model
  [row]

  (let [[id
         status
         _
         birth
         premium
         email
         fname
         sname] row]

    (cond-> {:id id
             :birth birth
             :email email
             :status (db/tr status)}

      (not= fname "N/A")
      (assoc :fname fname)

      (not= sname "N/A")
      (assoc :sname sname)

      premium
      (assoc :premium
             {:start (-> premium :account/premium :premium/start)
              :finish (-> premium :account/premium :premium/finish)}))))


(defn rows->models
  [rows limit birth]
  (->> rows
       (sort-by (row-sorter birth))
       (take limit)
       (map row->model)))


(defn sex-opposite
  [sex]
  (case sex
    :sex/f :sex/m
    :sex/m :sex/f))


(def fields
  [:diff :id :interests])

(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit id city country]} params

        account (db/pull [:account/interests
                          :account/sex
                          :account/birth]

                         [:account/id id])

        _ (when-not account
            (error/error 404 ""))

        interests (:account/interests account)
        birth     (:account/birth account)
        sex       (-> account :account/sex :db/ident)
        sex       (sex-opposite sex)

        params {:city city
                :country country
                :interests interests
                :sex sex}

        scope (params->scope params)
        {:keys [args]} scope

        query (dissoc scope :args :fields)

        _ (clojure.pprint/pprint query)
        _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil

        models (rows->models rows limit birth)]

    {:status 200
     :body {:accounts models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
