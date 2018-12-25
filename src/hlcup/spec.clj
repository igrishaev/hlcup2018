(ns hlcup.spec
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.set :as set]))


;;
;; Common
;;

(def invalid :clojure.spec.alpha/invalid)


(defmacro with-invalid
  [& body]
  `(try
     ~@body
     (catch Exception e#
       invalid)))


(s/def ::null
  (s/conformer
   (fn [value]
     (case value
       "0" 0
       "1" 1
       invalid))))


(s/def ::string string?)

(s/def ::non-empty-string
  (s/and ::string seq))


(s/def ::string-split
  (s/and
   string?
   (s/conformer
    (fn [value]
      (str/split value #",")))))

(defn ->int
  [^String value]
  (Integer/parseInt value))


(s/def ::->int
  (s/conformer
   (fn [value]
     (with-invalid
       (->int value)))))


(s/def ::int-split
  (s/and
   string?
   (s/conformer
    (fn [value]
      (with-invalid
        (mapv ->int (str/split value #",")))))))


(s/def ::is-one (partial = "1"))


(defmacro only-keys
  [& {:keys [req req-un opt opt-un] :as args}]
  `(s/and

    (s/map-of ~(set
                (concat
                 req
                 (map (comp keyword name) req-un)
                 opt
                 (map (comp keyword name) opt-un)))
              any?)

    (s/keys ~@(apply concat (vec args)))))

;;
;; Fields
;;

;;

(s/def ::query_id any?)

(s/def ::limit
  (s/and ::->int pos?))

;;

(s/def ::->sex
  (s/conformer
   (fn [value]
     (case value
       "m" :sex/m
       "f" :sex/f
       invalid))))

(s/def ::sex_eq ::->sex)

;;

(s/def ::email_domain ::string)
(s/def ::email_lt     ::string)
(s/def ::email_gt     ::string)

;;

(s/def ::->status
  (s/conformer
   (fn [value]
     (case value
       "свободны"   :status/free
       "заняты"     :status/busy
       "всё сложно" :status/complex
       invalid))))


(s/def ::status_eq  ::->status)
(s/def ::status_neq ::->status)

;;

(s/def ::fname_null ::null)
(s/def ::fname_eq   ::string)
(s/def ::fname_any  ::string-split)

;;

(s/def ::sname_eq     ::string)
(s/def ::sname_starts ::string)
(s/def ::sname_null   ::null)

;;

(s/def ::phone_code ::string)
(s/def ::phone_null ::null)

;;

(s/def ::country_eq   ::string)
(s/def ::country_null ::null)

;;

(s/def ::city_eq   ::string)
(s/def ::city_any  ::string-split)
(s/def ::city_null ::null)

;;

(s/def ::birth_lt   ::->int)
(s/def ::birth_gt   ::->int)
(s/def ::birth_year ::->int)

;;

(s/def ::interests_contains ::string-split)
(s/def ::interests_any      ::string-split)

;;

(s/def ::likes_contains ::int-split)

;;

(s/def ::premium_now  ::is-one)
(s/def ::premium_null ::null)

;;

(s/def :hlcup.api.filter/params
  (only-keys :opt-un [::query_id
                      ::limit

                      ::sex_eq

                      ::email_domain
                      ::email_lt
                      ::email_gt

                      ::status_eq
                      ::status_neq

                      ::fname_null
                      ::fname_eq
                      ::fname_any

                      ::sname_eq
                      ::sname_starts
                      ::sname_null

                      ::phone_code
                      ::phone_null

                      ::country_eq
                      ::country_null

                      ::city_eq
                      ::city_any
                      ::city_null

                      ::birth_lt
                      ::birth_gt
                      ::birth_year

                      ::interests_contains
                      ::interests_any

                      ::likes_contains

                      ::premium_now
                      ::premium_null]))


(s/def ::one
  (s/conformer
   (fn [value]
     (case value
       "-1" -1
       "1" 1
       invalid))))


(s/def ::order ::one)
(s/def ::sex       ::->sex)
(s/def ::email     ::string)
(s/def ::status    ::->status)
(s/def ::fname     ::string)
(s/def ::sname     ::string)
(s/def ::phone     ::string)
(s/def ::country   ::non-empty-string)
(s/def ::city      ::non-empty-string)
(s/def ::joined    ::->int)
(s/def ::birth     ::->int)
(s/def ::interests ::string)
(s/def ::likes     ::->int)


(def keys-enum
  #{"sex" "status" "interests" "country" "city"})

(s/def ::keys
  (s/and ::string-split
         (fn [values]
           (set/subset? (set values) keys-enum))))


(s/def :hlcup.api.group/params
  (only-keys

   :req-un [::keys]

   :opt-un [::query_id
            ::limit
            ::order

            ::sex
            ::email
            ::status
            ::fname
            ::sname
            ::phone
            ::country
            ::city
            ::birth
            ::joined
            ::interests
            ::likes]))


(s/def ::id ::->int)


(s/def :hlcup.api.recommend/params
  (only-keys
   :req-un [::limit
            ::id]

   :opt-un [::query_id
            ::country
            ::city]))


(s/def :hlcup.api.suggest/params
  (only-keys
   :req-un [::limit
            ::id]

   :opt-un [::query_id
            ::country
            ::city]))
