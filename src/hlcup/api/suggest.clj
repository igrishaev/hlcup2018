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


(defn xxx
  [values]
  (let [div (reduce + 0 values)]
    (/ 1 div)
    #_
    (if (zero? div)
      1
      (/ 1 div))))


(defn params->scope
  [params]

  (let [{:keys [targets
                mapping
                sex
                id
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
             '[(hlcup.db.func/diff ?ts1 ?ts2) ?diff])

       (push :in     '?mapping)
       (push :args   mapping))

      true
      (->
       (push :where '(not [?a :account/id ?id]))
       (push :in  '?id)
       (push :args id)

       (push :find '?a
             '(hlcup.api.suggest/xxx ?diff))
       (push :with '?like)))))


(defn map1
  [params]

  (let [{:keys [id]} params]

    (-> scope-base

        (push :find
              '?target '(avg ?ts))
        (push :where
              '[?a :account/id ?id]
              '[?a :account/likes ?like]
              '[?like :like/id ?target]
              '[?like :like/ts ?ts])

        (push :in     '?id)
        (push :args   id)

        db/scope

        )))


(defn map2
  [m1 params]

  (let [{:keys [sex
                country
                city]} params]

    (db/scope

     (cond-> scope-base

       true
       (->
        (push :where
              '[m1 ?target]
              '[?like :like/id ?target]
              '[?a :account/likes ?like])

        (push :in     'm1)
        (push :args   m1))

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
              '[?like :like/ts ?ts1]))

       true
       (->
        (push :find  '?a '(avg ?ts1) '?target))))))


(defn map3
  [m1 m2]

  (-> scope-base

      (push :find '?target '(hlcup.api.suggest/xxx ?diff)

            )

      (push :where
            '[m1 ?target ?ts1]
            '[m2 _ ?ts2 ?target]
            '[(hlcup.db.func/diff ?ts1 ?ts2) ?diff])

      (push :in    'm1 'm2)
      (push :args   m1 m2)

      db/scope))


#_
(defn map4
  []

  (db/scope

   (-> scope-base

       (push :find
             '?sim
             '?id
             '?email
             '?status
             '?sname
             '?fname)

       (push :where
             '[?a :account/likes ?like]
             '[?like :like/id ?u]
             '[(hlcup.api.suggest/yyy ?exclude ?u)]
             '[(clojure.core/get ?acc->sim ?a) ?sim]

             '[?u :account/id ?id]
             '[?u :account/email ?email]
             '[?u :account/status ?status]
             '[(get-else $ ?u :account/sname "N/A") ?sname]
             '[(get-else $ ?u :account/fname "N/A") ?fname])

       (push :args (keys acc->sim) (set exclude) acc->sim)
       (push :in '[?a ...] '?exclude '?acc->sim)))


  #_
  (-> scope-base

      (push :find '?target '(hlcup.api.suggest/xxx ?diff)

            )

      (push :where
            '[m1 ?target ?ts1]
            '[m2 _ ?ts2 ?target]
            '[(hlcup.db.func/diff ?ts1 ?ts2) ?diff])

      (push :in    'm1 'm2)
      (push :args   m1 m2)

      db/scope))


(def _p '[:account/id
          :account/email
          :account/status
          :accoount/fname])


(defn row->model
  [row]
  (let [[_
         id
         email
         status
         sname
         fname] row]

    (cond-> {:id id
             :email email
             :status (db/tr status)}

      (not= sname "N/A")
      (assoc :sname sname)

      (not= fname "N/A")
      (assoc :fname fname))))


(defn sorter
  [row]
  (let [[sim id] row]
    [(- sim) (- id)]))


(defn rows->models
  [rows limit]
  (->> rows
       (sort-by sorter)
       (take limit)
       (map row->model)))


(defn sex-opposite
  [sex]
  (case sex
    :sex/f :sex/m
    :sex/m :sex/f))


(def fields
  [:diff :id :interests])


(defn yyy
  [values value]
  (not (contains? values value)))


(defn get-target-likes
  [acc->sim exclude]

  (db/scope

   (-> scope-base

       (push :find
             '?sim
             '?id
             '?email
             '?status
             '?sname
             '?fname)

       (push :where
             '[?a :account/likes ?like]
             '[?like :like/id ?u]
             '[(hlcup.api.suggest/yyy ?exclude ?u)]
             '[(clojure.core/get ?acc->sim ?a) ?sim]

             '[?u :account/id ?id]
             '[?u :account/email ?email]
             '[?u :account/status ?status]
             '[(get-else $ ?u :account/sname "N/A") ?sname]
             '[(get-else $ ?u :account/fname "N/A") ?fname])

       (push :args (keys acc->sim) (set exclude) acc->sim)
       (push :in '[?a ...] '?exclude '?acc->sim))))


(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit id city country]} params

        account (db/pull [:account/id id])
        sex    (-> account :account/sex :db/ident)

        m1 (map1 {:id id})

        targets (map first m1)

        params {:city city
                :country country
                :targets targets
                :sex sex}

        m2 (map2 m1 params)
        m3 (map3 m1 m2)

        ;; m4 (map4 )

        ;; scope (params->scope params)

        ;; _ (clojure.pprint/pprint scope)

        ;; rows (db/scope scope)

        ]

    {:status 200
     :body {:accounts m3}}

    #_
    (if (seq rows)

      (let [acc->sim (dissoc (into {} rows) a)
            others (get-target-likes acc->sim targets)
            models (rows->models others limit)]

        {:status 200
         :body {:accounts models}})

      {:status 200
       :body {:accounts []}})))


(def handler
  (-> _handler
      (wrap-spec ::params)))
