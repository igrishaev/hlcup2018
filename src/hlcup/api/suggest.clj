(ns hlcup.api.suggest
  (:require
   [hlcup.spec]
   [hlcup.error :as error]
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


(def sum (partial reduce + 0))


(defn similarity
  [values]
  (sum (for [v values]
         (if (zero? v)
           0
           (/ 1 v)))))


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

      (push :find '?a '(hlcup.api.suggest/similarity ?diff))

      (push :where
            '[m1 ?target ?ts1]
            '[m2 ?a ?ts2 ?target]
            '[(hlcup.db.func/diff ?ts1 ?ts2) ?diff])

      (push :in    'm1 'm2)
      (push :args   m1 m2)

      db/scope))



(defn map4
  [_exclude m2 m3]

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
             '[m2 ?a]
             '[?a :account/likes ?like]
             '[?like :like/id ?u]
             '[(hlcup.api.suggest/exclude ?exclude ?u)]
             '[m3 ?a ?sim]

             '[?u :account/id ?id]
             '[?u :account/email ?email]
             '[?u :account/status ?status]
             '[(get-else $ ?u :account/sname "N/A") ?sname]
             '[(get-else $ ?u :account/fname "N/A") ?fname])

       (push :args (set _exclude) m2 m3)
       (push :in '?exclude 'm2 'm3))))


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


(defn exclude
  [values value]
  (not (contains? values value)))



(defn _handler
  [request]

  (let [{:keys [params]} request
        {:keys [limit id city country]} params

        account (db/pull [:account/id id])

        _ (when-not (:db/id account)
            (error/error 404 ""))

        sex    (-> account :account/sex :db/ident)

        m1 (map1 {:id id})

        targets (map first m1)

        params {:city city
                :country country
                :targets targets
                :sex sex}

        m2 (map2 m1 params)
        m3 (map3 m1 m2)

        m4 (map4 targets m2 m3)

        ]

    #_
    {:status 200
     :body m3}

    (if (seq m4)

      {:status 200
       :body {:accounts (rows->models m4 limit)}}

      {:status 200
       :body {:accounts []}})))


(def handler
  (-> _handler
      (wrap-spec ::params)))
