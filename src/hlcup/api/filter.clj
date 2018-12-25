(ns hlcup.api.filter
  (:require
   [hlcup.spec]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]
   [hlcup.time :as time2]

   [clojure.string :as str]))


(defn stub
  [name]
  (gensym (str "?_" name "_")))


(def scope-base
  '{:find []
    :in [$]
    :where []
    :args []
    :fields []})


(defmacro push
  [vector key & values]
  `(update ~vector ~key conj ~@values))


(defn item-found?
  [^clojure.lang.PersistentVector vector item]
  (-> vector (.indexOf item) (>= 0)))


(defn apply-defaults
  [scope]
  ;; todo check email

  (let [where '[?a :account/email ?email]
        found? (-> scope :where (item-found? where))]

    (cond-> scope
      (not found?)
      (push :where where)

      true
      (->
       (push :where  '[?a :account/id ?id])
       (push :fields :email :id)
       (push :find   '?email '?id)))))


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))

;;

(defmacro append
  [vector key values]
  `(update ~vector ~key into ~values))


(defmethod apply-predicate
  :sex_eq
  [scope _ value]
  (-> scope
      (push :where '[?a :account/sex ?sex])
      (push :fields :sex)
      (push :find  '?sex)
      (push :in    '?sex)
      (push :args  value)))

;;


;; todo types
(defn domain-match?
  [email domain]
  (str/ends-with? email (str "@" domain)))


(defmethod apply-predicate
  :email_domain
  ;; TODO: store domain!!!
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(hlcup.api.filter/domain-match? ?email ?_domain)])
      (push :in     '?_domain)
      (push :args   value)))


(defmethod apply-predicate
  :email_lt
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(< ?email ?_email)])
      (push :in     '?_email)
      (push :args   value)))


(defmethod apply-predicate
  :email_gt
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(> ?email ?_email)])
      (push :in     '?_email)
      (push :args   value)))

;;

(defmethod apply-predicate
  :status_eq
  [scope _ value]
  (-> scope
      (push :fields :status)
      (push :where
            ;; todo might be slow
            '[?a :account/status ?*status] ;; todo: drop that!
            '[?*status :db/ident ?status])

      (push :find   '?status)
      (push :in     '?status)
      (push :args   value)))


(defn status-opposite
  [status]
  (disj #{:status/free
          :status/busy
          :status/complex}
        status))


(defmethod apply-predicate
  :status_neq
  [scope _ value]
  ;; todo maybe or by two other

  (-> scope
      (push :fields :status)

      (push :where
            '(not [?a :account/status ?_status])
            '[?a :account/status ?*status]
            '[?*status :db/ident ?status])

      (push :find   '?status)
      (push :in     '?_status)
      (push :args   value)))

;;

(defmethod apply-predicate
  :fname_eq
  [scope _ value]
  (-> scope
      (push :fields :fname)
      (push :where  '[?a :account/fname ?fname])
      (push :find   '?fname)
      (push :in     '?fname)
      (push :args   value)))


(defmethod apply-predicate
  :fname_any
  [scope _ value]
  (-> scope
      (push :fields :fname)
      (push :where  '[?a :account/fname ?fname])
      (push :find   '?fname)
      (push :in     '[?fname ...])
      (push :args   value)))


(defmethod apply-predicate
  :fname_null
  [scope _ value]
  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/fname)]))

    0
    (-> scope
        (push :fields :fname)
        (push :where  '[?a :account/fname ?fname])
        (push :find   '?fname))))

;;

(defmethod apply-predicate
  :sname_eq
  [scope _ value]
  (-> scope
      (push :fields :sname)
      (push :where  '[?a :account/sname ?sname])
      (push :find   '?sname)
      (push :in     '?sname)
      (push :args   value)))


(defmethod apply-predicate
  :sname_starts
  [scope _ value]
  (-> scope
      ;; todo
      (push :fields :sname)
      (push :where
            '[?a :account/sname ?sname]
            '[(clojure.string/starts-with? ?sname ?_sname)])
      (push :find  '?sname)
      (push :in    '?_sname)
      (push :args  value)))


(defmethod apply-predicate
  :sname_null
  [scope _ value]
  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/sname)]))

    0
    (-> scope
        (push :fields :sname)
        (push :where  '[?a :account/sname ?sname])
        (push :find   '?sname))))

;;

;; todo types
(defn phone-matches?
  [phone code]
  (= (subs phone 2 5) code))


(defmethod apply-predicate
  :phone_code
  [scope _ value]
  (-> scope
      ;; todo store code
      (push :fields :phone)
      (push :where
            '[?a :account/phone ?phone]
            '[(hlcup.api.filter/phone-matches? ?phone ?code)])
      (push :find  '?phone)
      (push :in    '?code)
      (push :args  value)))


(defmethod apply-predicate
  :phone_null
  [scope _ value]
  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/phone)]))

    0
    (-> scope
        (push :fields :phone)
        (push :where  '[?a :account/phone ?phone])
        (push :find   '?phone))))

;;


(defmethod apply-predicate
  :country_eq
  [scope _ value]
  (-> scope
      (push :fields :country)
      (push :where  '[?a :account/country ?country])
      (push :find   '?country)
      (push :in     '?country)
      (push :args   value)))


(defmethod apply-predicate
  :country_null
  [scope _ value]
  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/country)]))

    0
    (-> scope
        (push :fields :country)
        (push :where  '[?a :account/country ?country])
        (push :find   '?country))))

;;

(defmethod apply-predicate
  :city_eq
  [scope _ value]
  (-> scope
      (push :fields :city)
      (push :where  '[?a :account/city ?city])
      (push :find   '?city)
      (push :in     '?city)
      (push :args   value)))


(defmethod apply-predicate
  :city_any
  [scope _ value]
  (-> scope
      (push :fields :city)
      (push :where  '[?a :account/city ?city])
      (push :find   '?city)
      (push :in     '[?city ...])
      (push :args   value)))


(defmethod apply-predicate
  :city_null
  [scope _ value]
  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/city)]))

    0
    (-> scope
        (push :fields :city)
        (push :where  '[?a :account/city ?city])
        (push :find   '?city))))

;;

(defmethod apply-predicate
  :birth_lt
  [scope _ value]
  (-> scope
      (push :fields :birth)
      (push :where
            '[?a :account/birth ?birth]
            '[(< ?birth ?_birth)])
      (push :find  '?birth)
      (push :in    '?_birth)
      (push :args  value)))


(defmethod apply-predicate
  :birth_gt
  [scope _ value]
  (-> scope
      (push :fields :birth)
      (push :where
            '[?a :account/birth ?birth]
            '[(> ?birth ?_birth)])
      (push :find  '?birth)
      (push :in    '?_birth)
      (push :args  value)))






(defmethod apply-predicate
  :birth_year
  [scope _ value]

  (let [[ts1 ts2] (time2/ts->range value)]
    (-> scope
        (push :fields :birth)
        (push :where
              '[?a :account/birth ?birth]
              '[(<= ?birth1 ?birth )]
              '[(<= ?birth  ?birth2)])
        (push :find  '?birth)
        (push :in    '?birth1 '?birth2)
        (push :args  ts1 ts2))))

;;

(defmethod apply-predicate
  :interests_contains
  [scope _ value]

  (let [new-sym (partial stub "interest")
        symbols (repeatedly (count value) new-sym)]

    (-> scope
        (append :where
                (for [sym symbols]
                  ['?a :account/interests sym]))
        (append :in   symbols)
        (append :args value))))


(defmethod apply-predicate
  :interests_any
  [scope _ value]
  (-> scope
      (push :where '[?a :account/interests ?interest])
      (push :in    '[?interest ...])
      (push :args  value)))

;;

(defn _accs
  [ids]

  (let [refs (for [id ids]
               [:account/id id])

        result
        (db/query '{:find [(distinct ?_id) ?a]
                     :in [$ [?account ...] ],
                     :where
                     [[?like :like/id ?account]
                      [?a :account/likes ?like]
                      [?account :account/id ?_id]]}
                  [refs])

        ]
    (map second (get (group-by first result) (set ids)))))




(defmethod apply-predicate
  :likes_contains
  [scope _ value]

  (let [accs (_accs value)]

    (-> scope
        (push :where '[?a :account/id _])
        (push :in '[?a ...])
        (push :args accs))))

;;

(def NOW 1545699626)


(defmethod apply-predicate
  :premium_now
  [scope _ value]
  (-> scope

      ;; todo now
      ;; todo premium storage
      ;; todo between func
      (push :fields
            :start :finish)

      (push :where
            '[?a :account/premium ?premium]
            '[?premium :premium/start ?start]
            '[?premium :premium/finish ?finish]
            '[(<= ?start ?now)]
            '[(<= ?now ?finish)])

      (push :find
            '?start
            '?finish)

      (push :in '?now)
      (push :args NOW)))


(defmethod apply-predicate
  :premium_null
  [scope _ value]

  (condp = value

    1
    (-> scope
        (push :where '[(missing? $ ?a :account/premium)]))

    0
    (-> scope
        ;; todo premium
        (push :fields :start :finish)

        (push :where
              '[?a :account/premium ?premium]
              '[?premium :premium/start ?start]
              '[?premium :premium/finish ?finish])

        (push :find '?start '?finish))))

;;


(defn params->scope
  [params]

  (reduce-kv
   (fn [scope predicate value]
     (apply-predicate scope predicate value))
   scope-base
   params))


(defn ->model
  [fields row]
  (let [model (zipmap fields row)
        {:keys [sex status]} model]
    (cond-> model

      status
      (update :status db/tr)

      sex
      (update :sex db/tr))))


(defn fix-premium
  [model]
  (let [{:keys [start finish]} model]
    (if (or start finish)
      (-> model
          (dissoc :start :finish)
          (assoc :premium {:start start
                           :finish finish}))
      model)))


(defn rows->models
  [rows fields limit]
  ;; todo use transducers
  (->> rows
       (sort-by (comp - peek))
       (take limit)
       (map (partial ->model fields))
       ;; todo make it conditional
       (map fix-premium)))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit]} params

        params (dissoc params :limit :query_id)

        scope (-> params params->scope apply-defaults)
        {:keys [args fields]} scope

        query (dissoc scope :args :fields)

        ;; _ (clojure.pprint/pprint query)
        ;; _ (clojure.pprint/pprint args)

        rows (db/query query args)

        ;; todo limit nil
        models (rows->models rows fields limit)]

    {:status 200
     :body {:accounts models}}))


(def handler
  (-> _handler
      (wrap-spec ::params)))
