(ns hlcup.api-group)


(def query-filter
  '{:find
    [?id ?email]

    :in [$]

    :args []

    :where
    [[?a :account/id ?id]
     [?a :account/email ?email]]})


(defmulti apply-predicate
  (fn [predicate value query]
    predicate))


(defmethod apply-predicate
  :sex
  [_ value query]
  (-> query
      (update :where conj '[?a :account/sex ?sex])
      (update :find conj '?sex)
      (update :in conj '?sex)
      (update :args conj value)))


(defmethod apply-predicate
  :status
  [_ value query]
  (-> query
      (update :where conj '[?a :account/status ?status])
      (update :find conj '?status)
      (update :in conj '?status)
      (update :args conj value)))


(defmethod apply-predicate
  :fname
  [_ value query]
  (-> query
      (update :where conj '[?a :account/fname ?fname])
      (update :find conj '?fname)
      (update :in conj '?fname)
      (update :args conj value)))


(defmulti apply-key
  (fn [key query]
    key))


(defmethod apply-key
  "sex"
  [_ query]
  (-> query
      (update :where conj '[?a :account/fname ?sex])
      (update :find conj '?sex)))


(defmethod apply-key
  "status"
  [_ query]
  (-> query
      (update :where conj '[?a :account/status ?status])
      (update :find conj '?status)))


(defmethod apply-key
  "interests"
  [_ query]
  (-> query
      (update :where conj '[?a :account/interests ?interest])
      (update :find conj '?interest)))


(defmethod apply-key
  "country"
  [_ query]
  (-> query
      (update :where conj
              '[(get-else $ ?a :account/country "N/A") ?country])
      (update :find conj '?country)))


(defmethod apply-predicate
  :keys
  [_ value query]
  (reduce
   (fn [query key]
     (apply-key key query))
   query
   value)


  (-> query
      (update :where conj '[?a :account/fname ?fname])

      (update :find conj
              '()


              )

      (update :in conj '?fname)
      (update :args conj value)))
