(ns hlcup.api.group
  (:require
   hlcup.spec
   [hlcup.time :as time]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


(defn stub
  [name]
  (gensym (str "?_" name "_")))


(defmacro push
  [vector key & values]
  `(update ~vector ~key conj ~@values))


(def scope-base
  '{:find []
    :in [$]
    :where []
    :args []
    :fields []})


(defmulti apply-group
  (fn [scope field]
    field))


;; todo indexOf reflect
(defn item-found?
  [^clojure.lang.PersistentVector vector item]
  (-> vector (.indexOf item) (>= 0)))


(defmethod apply-group
  "sex"
  [scope _]

  (let [where '[?a :account/sex ?sex]
        found? (-> scope :where (item-found? where))]

    (cond-> scope

      true
      (->
       (push :fields :sex)
       (push :find '?sex))

      (not found?)
      (push :where where))))


(defmethod apply-group
  "status"
  [scope _]

  (let [where '[?a :account/status ?status]
        found? (-> scope :where (item-found? where))]

    (cond-> scope

      true
      (->
       (push :fields :status)
       (push :find '?status))

      (not found?)
      (push :where where))))


(defmethod apply-group
  "interests"
  [scope _]

  (let [where '[?a :account/interests ?interests]
        found? (-> scope :where (item-found? where))]

    (cond-> scope

      true
      (->
       (push :fields :interests)
       (push :find '?interests))

      (not found?)
      (push :where where))))


(defn account-linked?
  [scope]
  (-> scope :where first first (= '?a)))


(defmethod apply-group
  "country"
  [scope _]

  (let [where '[?a :account/country ?country]
        found? (-> scope :where (item-found? where))
        linked? (account-linked? scope)]

    (cond-> scope

      true
      (->
       (push :fields :country)
       (push :find '?country))

      (not found?)
      (cond->
          ;; TODO slow
          (not linked?)
          (push :where '[?a :account/id _])

          true
          (push :where
                '[(get-else $ ?a :account/country "N/A") ?country])))))



(defmethod apply-group
  "city"
  [scope _]

  (let [where '[?a :account/city ?city]
        found? (-> scope :where (item-found? where))
        linked? (account-linked? scope)]

    (cond-> scope

      true
      (->
       (push :fields :city)
       (push :find   '?city))

      (not found?)
      (cond->

        ;; TODO: that's too slow!
        (not linked?)
        (push :where '[?a :account/id _])

        true
        (push :where
              '[(get-else $ ?a :account/city "N/A") ?city])))))


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))


(defmethod apply-predicate
  :keys
  [scope _ value]
  (reduce
   (fn [scope field]
     (apply-group scope field))
   scope
   value))


(defn apply-filter
  [scope field value]
  (let [alias (name field)
        ?bind (symbol (str "?" alias))
        where (vector '?a (keyword "account" alias) ?bind)]
    (-> scope
        (push :where where)
        (push :in    ?bind)
        (push :args  value))))


(defmethod apply-predicate
  :sex
  [scope _ value]
  (apply-filter scope :sex value))


(defmethod apply-predicate
  :email
  [scope _ value]
  (apply-filter scope :email value))


(defmethod apply-predicate
  :status
  [scope _ value]
  (apply-filter scope :status value))


(defmethod apply-predicate
  :fname
  [scope _ value]
  (apply-filter scope :fname value))


(defmethod apply-predicate
  :sname
  [scope _ value]
  (apply-filter scope :sname value))


(defmethod apply-predicate
  :phone
  [scope _ value]
  (apply-filter scope :phone value))


(defmethod apply-predicate
  :country
  [scope _ value]
  (apply-filter scope :country value))


(defmethod apply-predicate
  :city
  [scope _ value]
  (apply-filter scope :city value))


;; todo year
;; todo between
(defmethod apply-predicate
  :birth
  [scope _ value]

  (let [[ts1 ts2] (time/ts->range value)]
    (-> scope
        (push :where
              '[?a :account/birth ?birth]
              '[(<= ?birth1 ?birth )]
              '[(<= ?birth  ?birth2)])

        (push :in
              '?birth1 '?birth2)

        (push :args
              ts1 ts2)))
)


;; todo store year
(defmethod apply-predicate
  :joined
  [scope _ value]
  (let [[ts1 ts2] (time/ts->range value)]
    (-> scope
        (push :where
              '[?a :account/joined ?joined]
              '[(<= ?joined1 ?joined )]
              '[(<= ?joined  ?joined2)])

        (push :in
              '?joined1 '?joined2)

        (push :args
              ts1 ts2))))


;; todo single
(defmethod apply-predicate
  :interests
  [scope _ value]
  (apply-filter scope :interests value))


;; TODO: weights!


;; todo single
(defmethod apply-predicate
  :likes
  [scope _ value]

  ;; todo might be slow
  (-> scope
      (push :where
            '[?a :account/likes ?like]
            '[?like :like/id ?target])

      (push :in '?target)
      (push :args [:account/id value])))


(defn apply-defaults
  [scope]
  (-> scope
      (push :find   '(count ?a))
      (push :fields :count)))


(defn apply-predicates
  [scope params]
  (reduce-kv
   (fn [scope predicate value]
     (apply-predicate scope predicate value))
   scope
   (dissoc params :keys)))


(defn apply-keys
  [scope params]
  (let [{:keys [keys]} params]
    (apply-predicate scope :keys keys)))


(defn params->scope
  [params]
  (-> scope-base
      (apply-predicates params)
      (apply-keys params)
      (apply-defaults)))


(defn ->model
  [fields row]
  (let [model (zipmap fields row)
        {:keys [sex country city status]} model]

    (cond-> model

      (= country "N/A")
      (dissoc :country)

      (= city "N/A")
      (dissoc :city)

      sex
      (update :sex db/tr)

      status
      (update :status db/tr))))


;; todo improve
(defn apply-order
  [order keys rows]

  (let [juxter (apply juxt :count (map keyword keys))]

    (cond-> (sort-by juxter rows)

      (= order -1)
      (reverse))))


;; todo N/A to nil
;; todo limit nil
;; todo order direction
;; todo transducers
(defn rows->models
  [rows fields order limit keys]
  (->> rows
       (map (partial ->model fields))

       (apply-order order keys)
       (take limit)

       #_
       (map (partial ->model fields))))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit order keys]} params

        params (dissoc params :limit :order :query_id)

        scope (params->scope params)
        {:keys [args fields]} scope

        query (dissoc scope :args :fields)

        ;; _ (clojure.pprint/pprint query)
        ;; _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil

        models (rows->models rows fields order limit keys)]

    {:status 200
     :body {:groups models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
