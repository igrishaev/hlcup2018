(ns hlcup.spec.filter
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))


;;
;; Common
;;

(def invalid :clojure.spec.alpha/invalid)


(defmacro with-invalid
  [& body]
  `(try
     ~@body
     (catch Exception e#
       :clojure.spec.alpha/invalid)))

(s/def ::null
  (s/and
   #{"0" "1"}
   (s/conformer
    (fn [value]
      (case value
        "0" 0
        "1" 1
        invalid)))))


(s/def ::string string?)

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


(s/def ::one (partial = "1"))


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

(s/def ::fname_null ::null)
(s/def ::fname_eq ::string)
(s/def ::fname_any ::string-split)


(s/def ::interests_contains ::string-split)
(s/def ::interests_any ::string-split)

(s/def ::likes_contains ::int-split)

(s/def ::premium_now ::one)
(s/def ::premium_null ::null)

(s/def ::limit
  (s/and ::string ::->int))


(s/def ::sex_eq
  #{"m" "f"})


(s/def ::params
  (only-keys :opt-un [::query_id
                      ::fname_eq
                      ::limit

                      ::sex_eq
                      ::interests_any

                      ]))
