(ns hlcup.api.filter
  (:require
   [hlcup.spec.filter]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


(def scope-base
  '{:find
    [?id ?email]

    :in [$]

    :where
    [[?a :account/id ?id]
     [?a :account/email ?email]]

    :args []
    :fields [:id :email]})


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))


(defmethod apply-predicate
  :default
  [scope _ _]
  scope)


(defn ->sex
  [value]
  (case value
    "m" :sex/m
    "f" :sex/f))


(defmethod apply-predicate
  :sex_eq
  [scope _ value]
  (-> scope
      (update :where conj '[?a :account/sex ?sex])
      (update :fields conj :sex)
      (update :find conj '?sex)
      (update :in conj '?sex)
      (update :args conj (->sex value))))


(defmethod apply-predicate
  :status_eq
  [scope _ value]
  (-> scope
      (update :where conj '[?a :account/status ?status])
      (update :find conj '?status)
      (update :in conj '?status)
      (update :args conj value)))


(defmethod apply-predicate
  :status_neq
  [scope _ value]
  (-> scope
      ;; todo: maybe search by two other statuses
      (update :where conj '(not [?a :account/status ?status]))
      (update :find conj '?status)
      (update :in conj '?status)
      (update :args conj value)))


(defmethod apply-predicate
  :fname_eq
  [scope _ value]
  (-> scope
      (update :where conj '[?a :account/fname ?fname])
      (update :find conj '?fname)
      (update :in conj '?fname)
      (update :args conj value)))


(defmethod apply-predicate
  :fname_any
  [scope _ value]
  (-> scope
      (update :where conj '[?a :account/fname ?fname])
      (update :find conj '?fname)
      (update :in conj '[?fname ...])
      (update :args conj value)))


(defmethod apply-predicate
  :fname_null
  [scope _ value]

  ;; todo warning
  #_
  (case value
    0
    (-> scope
        (update :where conj '[(missing? $ ?a :account/fname)]))

    1
    (-> scope
        (update :where conj '[?a :account/fname ?fname])
        (update :find conj '?fname)
        (update :in conj '?fname)
        (update :args conj value))))


(defmethod apply-predicate
  :interests_any
  [scope _ value]
  (-> scope
      (update :where conj '[?a :account/interests ?interest])
      (update :in conj '[?interest ...])
      (update :args conj value)))


(defmethod apply-predicate
  :interests_contains
  [scope _ value]

  (let [new-sym #(gensym "?interest")
        symbols (repeatedly (count value) new-sym)]

    (-> scope
        (update :where into
                (for [sym symbols]
                  ['?a :account/interests sym]))
        (update :in into symbols)
        (update :args into value))))


(defmethod apply-predicate
  :likes_contains
  [scope _ value]

  (let [new-sym #(gensym "?account")
        symbols (repeatedly (count value) new-sym)
        ref-seq (for [id value]
                  [:account/id id])]

    (-> scope

        (update :where conj '[?a :account/likes ?like])
        (update :where into
                (for [sym symbols]
                  ['?like :like/id sym]))
        (update :in into symbols)
        (update :args into ref-seq))))


(defmethod apply-predicate
  :premium_now
  [scope _ value]
  (-> scope

      ;; todo premium storage
      (update :where conj
              '[?a :account/premium ?premium]
              '[?premium :premium/start ?start]
              '[?premium :premium/finish ?finish]
              '[(< ?start ?now ?finish)])

      (update :find conj
              '?start
              '?finish)

      (update :in conj
              '?now)

      ;; todo deal with premium
      (update :args conj
              111111111)))


(defmethod apply-predicate
  :premium_null
  [scope _ value]

  #_
  (case value

    0
    (-> scope
        (update :where conj
                '[(missing? $ ?a :account/premium)]))

    1
    (-> scope
        (update :where conj
                '[?a :account/premium ?premium]
                '[?premium :premium/start ?start]
                '[?premium :premium/finish ?finish])

        (update :find conj '?fname)
        (update :in conj '?fname)
        (update :args conj value))))



(defn params->scope
  [params]

  (reduce-kv
   (fn [scope predicate value]
     (apply-predicate scope predicate value))
   scope-base
   params))



(defn ->model
  [fields row]
  (zipmap fields row))


(defn rows->models
  [rows fields limit]
  ;; todo use transducers
  (->> rows
       (sort-by (comp - first))
       (take limit)
       #_
       (map (partial ->model fields))))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit]} params

        params (dissoc params :limit :query_id)

        scope (params->scope params)
        {:keys [args fields]} scope

        query (dissoc scope :args :fields)

        _ (clojure.pprint/pprint query)
        _ (clojure.pprint/pprint args)

        rows (db/query query args)
        ]

    {:status 200
     :body {:accounts rows}}))


(def handler
  (-> _handler
      (wrap-spec :hlcup.spec.filter/params)))
