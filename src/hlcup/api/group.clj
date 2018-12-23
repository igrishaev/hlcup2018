(ns hlcup.api.group
  (:require
   hlcup.spec
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]))


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


(defmethod apply-group
  "sex"
  [scope _]
  (-> scope
      (push :where '[?a :account/sex ?sex])
      (push :fields :sex)
      (push :find '?sex)))


(defmethod apply-group
  "status"
  [scope _]
  (-> scope
      (push :where '[?a :account/status ?status])
      (push :fields :status)
      (push :find '?status)))


(defmethod apply-group
  "interests"
  [scope _]
  (-> scope
      (push :where '[?a :account/interests ?interest])
      (push :fields :interests)
      (push :find '?interest)))



(defmethod apply-group
  "country"
  [scope _]
  (-> scope
      (push :where
            '[(get-else $ ?a :account/country "N/A") ?country])
      (push :fields :country)
      (push :find '?country)))


(defmethod apply-group
  "city"
  [scope _]
  (-> scope
      (push :where
            '[(get-else $ ?a :account/city "N/A") ?city])
      (push :fields :city)
      (push :find '?city)))


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))


(defmethod apply-predicate
  :keys
  [scope _ value]
  (reduce
   (fn [scope field]
     (apply-group scope field))
   scope-base
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
(defmethod apply-predicate
  :birth
  [scope _ value]
  (apply-filter scope :birth value))


;; todo year
(defmethod apply-predicate
  :joined
  [scope _ value]
  (apply-filter scope :joined value))


;; todo single
(defmethod apply-predicate
  :interests
  [scope _ value]
  (apply-filter scope :interests value))


;; todo single
(defmethod apply-predicate
  :likes
  [scope _ value]
  (apply-filter scope :likes value))


(defn apply-defaults
  [scope]
  (-> scope
      (push :find   '(count ?a))
      (push :fields :count)))


(defn params->scope
  [params]

  (reduce-kv
   (fn [scope predicate value]
     (apply-predicate scope predicate value))
   scope-base
   params))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit]} params

        params (dissoc params :limit :order :query_id)

        scope (-> params params->scope apply-defaults)
        {:keys [args fields]} scope

        query (dissoc scope :args :fields)

        ;; _ (clojure.pprint/pprint query)
        ;; _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil
        ;; models (rows->models rows fields limit)

        ]

    {:status 200
     :body rows

     #_
     {:accounts models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
