(ns hlcup.api.filter
  (:require
   [hlcup.spec]
   [hlcup.middleware :refer [wrap-spec]]
   [hlcup.db :as db]

   [clj-time.core :as time]
   [clj-time.coerce :as coerce]

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


(defn apply-defaults
  [scope]
  ;; todo check email
  (-> scope
      (update :where conj
              '[?a :account/email ?email]
              '[?a :account/id ?id])

      (update :fields conj
              :email :id)

      (update :find conj
              '?email '?id)))


(defmulti apply-predicate
  (fn [scope predicate value]
    predicate))

;;

(defmacro push
  [vector key & values]
  `(update ~vector ~key conj ~@values))


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
  ;; todo: store domain
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(hlcup.api.filter/domain-match? ?email ?_domain)])

      (push :fields :email)
      (push :find   '?email)
      (push :in     '?_domain)
      (push :args   value)))


(defmethod apply-predicate
  :email_lt
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(< ?email ?_email)])

      (push :fields :email)
      (push :find   '?email)
      (push :in     '?_email)
      (push :args   value)))


(defmethod apply-predicate
  :email_gt
  [scope _ value]
  (-> scope
      (push :where
            '[?a :account/email ?email]
            '[(> ?email ?_email)])

      (push :fields :email)
      (push :find   '?email)
      (push :in     '?_email)
      (push :args   value)))

;;

(defmethod apply-predicate
  :status_eq
  [scope _ value]
  (-> scope
      (push :fields :status)
      (push :where  '[?a :account/status ?status])
      (push :find   '?status)
      (push :in     '?status)
      (push :args   value)))


(defmethod apply-predicate
  :status_neq
  [scope _ value]
  ;; todo maybe or by two other
  (-> scope
      (push :fields :status)
      (push :where  '(not [?a :account/status ?status]))
      (push :find   '?status)
      (push :in     '?status)
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

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/fname)]))

    1
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

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/sname)]))

    1
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

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/phone)]))

    1
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

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/country)]))

    1
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

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/city)]))

    1
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


;; todo: compare timestamps; calc low/high
;; todo types
(defn timestamp-year?
  [timestamp year]
  (-> timestamp
      (* 1000)
      coerce/from-long
      time/year
      (= year)))


(defmethod apply-predicate
  :birth_year
  [scope _ value]
  ;; todo store year
  ;; todo move query funcs
  (-> scope
      (push :fields :birth)
      (push :where
            '[?a :account/birth ?birth]
            '[(hlcup.api.filter/timestamp-year? ?birth ?year)])
      (push :find  '?birth)
      (push :in    '?_year)
      (push :args  value)))

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

(defmethod apply-predicate
  :likes_contains
  [scope _ value]

  (let [new-sym (partial stub "account")
        symbols (repeatedly (count value) new-sym)]

    (-> scope

        (push :where '[?a :account/likes ?like])
        (append :where
                (for [sym symbols]
                  ['?like :like/id sym]))

        (append :in symbols)
        (append :args
                (for [id value]
                  [:account/id id])))))

;;


(defmethod apply-predicate
  :premium_now
  [scope _ value]
  (-> scope

      ;; todo now
      ;; todo premium storage
      (push :fields
            :start :finisj)

      (push :where
            '[?a :account/premium ?premium]
            '[?premium :premium/start ?start]
            '[?premium :premium/finish ?finish]
            '[(< ?start ?now ?finish)])

      (push :find
            '?start
            '?finish)

      (push :in '?now)
      (push :args 111111111)))


(defmethod apply-predicate
  :premium_null
  [scope _ value]

  (condp = value

    0
    (-> scope
        (push :where '[(missing? $ ?a :account/premium)]))

    1
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
  (zipmap fields row))


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
