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
            '[?a :account/email  ?email]
            '[?a :account/status ?status]
            '[(get-else $ ?a :account/fname "N/A") ?fname]
            '[(get-else $ ?a :account/sname "N/A") ?sname])

      (push :find
            '?id
            '?status
            '(count ?interests)
            '?email
            '?fname
            '?sname)))


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
  :birth
  [scope _ value]
  (-> scope
      (push :find
            '?birth '?diff)
      (push :where
            '[?a :account/birth ?birth]
            '[(hlcup.db.func/diff ?birth ?_birth) ?diff])
      (push :in     '?_birth)
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
      (->
       (apply-predicate :birth 12312312)
       (apply-defaults)))))


(defn row-sorter
  [row]
  (let [[_ ;; birth
         diff
         id
         status
         interests
         _ ;;email
         _ ;;fname
         _ ;;sname
         ] row]

    [;; prem

     status
     (- interests)
     (- diff)
     (- id)

     ]

    )

  )


(defn id->status
  [id]
  (case id
    17592186045419 "свободны"
    17592186045420 "заняты"
    17592186045421 "всё сложно"))


(defn row->model
  [row]

  (let [[birth
         _ ;; diff
         id
         status
         _ ;; interests
         email
         fname
         sname
         ] row]

    (cond-> {:id id
             :birth birth
             :email email
             :status (id->status status)}

      (not= fname "N/A")
      (assoc :fname fname)

      (not= sname "N/A")
      (assoc :sname sname))))


(defn rows->models
  [rows limit]
  (->> rows
       (sort-by row-sorter)
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
        birth     (:account/sex account)
        sex       (-> account :account/sex :db/ident)
        sex       (sex-opposite sex)

        params {:city city
                :country country
                :interests interests
                :sex sex
                :birth birth}

        scope (params->scope params)
        {:keys [args]} scope

        query (dissoc scope :args :fields)

        _ (clojure.pprint/pprint query)
        _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil

        models (rows->models rows limit)]

    {:status 200
     :body {:accounts models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
