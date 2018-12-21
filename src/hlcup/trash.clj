(defn foo
  []

  (d/q

   '[:find

     ?target-id
     ?other-id

     (sum ?diff)

     ;; (max ?account-ts)
     ;; (avg ?other-ts)

     :in $ ?account

     :with
     ?account-ts
     ?other-ts

     :where

     [?account        :account/likes  ?account-like]
     [?account-like   :like/id        ?account-target]
     [?account-like   :like/ts        ?account-ts]

     [?other-like     :like/id        ?account-target]
     [?account-target :account/id     ?target-id]

     [?other-like     :like/ts        ?other-ts]
     [?other-account  :account/likes  ?other-like]
     [?other-account  :account/id     ?other-id]

     [(hlcup.db/ggg ?account-ts ?other-ts) ?diff]


     ]

   (d/db conn)
   [:account/id 1]
   ))


(defn bar
  []

  (d/q

   '[:find

     ?sex
     ?country
     (count ?account)

     :in
     $
     ;; ?account

     :where

     [?account        :account/sex      ?sex]
     [(get-else $ ?account :account/country "") ?country]

     ]

   (d/db conn)
   ;; [:account/id 4066]

   ))

(defn baz
  []

  (d/q

   '[:find ?id
     :in $
     :where
     [?account        :account/id      ?id]

     not null
     [?account :account/country _]

     null
     [(missing? $ ?account :account/country)]

     ]

   (d/db conn)

   ))


(defn ccc
  [values]

  (reduce
   (fn [result value]
     (if value
       (inc result)
       result))
   0
   values))


(defn aaa
  []

  (d/q

   '[:find

     ?id

     ;; (distinct ?interest)
     ;; (distinct ?other-interest)

     ?other

     (hlcup.db/ccc ?match)

     :in
     $
     ?account
     ?country

     :where

     [?account        :account/id          ?id]
     [?account        :account/interests   ?interest]

     [?account        :account/birth       ?birth]

     [?other          :account/sex         :sex/m]
     ;; [?other          :account/country     ?country]

     [?other          :account/interests   ?other-interest]
     [?other          :account/birth       ?other-birth]

     [(= ?other-interest ?interest) ?match]


     ]

   (d/db conn)
   [:account/id 1]
   "Гератрис"
   ))


(defn a
  []
  (d/q

   '[:find ?id
     :in $
     :where
     [?a :account/id ?id]
     [?a :account/interests "50 Cent"]]

   (d/db conn)))


(defn b
  []
  (d/q

   '[:find ?id
     :in $ ?iii
     :where
     [?a :account/id ?id]
     [(datomic.api/entity $ ?a) ?e]
     [(:account/interests ?e) ?interests]
     [(clojure.set/superset? ?interests ?iii)]]

   (d/db conn)
   #{"50 Cent"}


   ))


(defn c
  []
  (d/q

   '[:find ?id
     :in $ ?iii
     :where
     [?a :account/id ?id]
     [(datomic.api/entity $ ?a) ?e]
     [(:account/interests ?e) ?interests]
     [(hlcup.db/superset? ?interests ?iii)]]

   (d/db conn)
   #{"50 Cent" "Выходные" "Кинокифильмы"}


   ))


(defn d
  []
  (d/q

   '[:find (count ?id)
     :in $
     :where
     [?a :account/id ?id]]

   (d/db conn)))


(defn e
  []

  (d/q

   '[:find ?id
     :in $ [?interest ...]
     :where
     [?a :account/id ?id]
     (not-join [?a ?interest]
               [?a :account/interests ?i]
               (not [(= ?i ?interest)]))]

   (d/db conn)
   #{"50 Cent" "Выходные" "Кинокифильмы" "Солнце" "Целоваться"}

   ))
