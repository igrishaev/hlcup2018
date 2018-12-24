(ns hlcup.api.suggest
  (:require
   [hlcup.spec]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


(def scope-base
  '{:find []
    :in [$]
    :with []
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
            '[?a :account/fname  ?fname]
            '[?a :account/sname  ?sname])

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




(defn xxx
  [values]
  (let [div (reduce + 0 values)]
    (if (zero? div)
      1
      (/ 1 div))))


(defn params->scope
  [params]

  (let [{:keys [targets
                mapping
                sex
                country
                city]} params]

    (cond-> scope-base

      true
      (->
       (push :where
             '[?like :like/id ?target]
             '[?a :account/likes ?like])

       (push :in     '[?target ...])
       (push :args   targets))

      city
      (->
       (push :where  '[?a :account/city ?city])
       (push :in     '?city)
       (push :args   city))

      country
      (->
       (push :where  '[?a :account/country ?country])
       (push :in     '?country)
       (push :args   country))

      sex
      (->
       (push :where  '[?a :account/sex ?sex])
       (push :in     '?sex)
       (push :args   sex))

      true
      (->
       (push :where
             '[?like :like/ts ?ts1]
             '[(clojure.core/get ?mapping ?target 0) ?ts2]
             '[(hlcup.db.func/diff ?ts1 ?ts2) ?diff]


             )
       (push :in     '?mapping)
       (push :args   mapping))

      true
      (->
       (push :find '?a '(hlcup.api.suggest/xxx ?diff))
       (push :with '?like)))))


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
    17592186045421 "все сложно"))


(def _p '[:account/id
          :account/email
          :account/status
          :accoount/fname])


(defn row->model
  [row]
  (let [[account _] row

        e (db/pull _p account)]

    {:id (:account/id e)
     :email (:account/email e)
     :status (-> e :account/status :db/ident)
     :fname (:accoount/fname e)}))


(defn rows->models
  [rows limit]
  (->> rows
       (sort-by (comp - peek))
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

        account (db/pull [:account/likes
                          :account/sex]

                         [:account/id id])

        likes  (:account/likes account)
        sex    (-> account :account/sex :db/ident)

        mapping (into {}
                      (for [like likes
                            :let [account (-> like :like/id :db/id)
                                  timestamp (-> like :like/ts)]]
                        [account timestamp]))

        targets (-> mapping keys set)

        params {:city city
                :country country
                :mapping mapping
                :targets targets
                :sex sex}

        scope (params->scope params)
        {:keys [args]} scope

        query (dissoc scope :args :fields)

        ;; _ (clojure.pprint/pprint query)
        ;; _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil

        models (rows->models rows limit)]

    {:status 200
     :body {:accounts models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
