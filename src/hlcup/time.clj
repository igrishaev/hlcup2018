(ns hlcup.time
  (:require
   [clj-time.core :as time]
   [clj-time.coerce :as coerce]))


(defn ymd->ts
  [y m d]
  (coerce/to-epoch
   (time/date-time y m d)))


(defn ts->year
  [timestamp]
  (-> timestamp
      coerce/from-epoch
      time/year))


(defn ts->range
  [year]
  [(-> year
       (time/date-time 1 1)
       (coerce/to-epoch))
   (-> year
       (time/date-time 12 31 23 59 59)
       (coerce/to-epoch))])


(comment
  (ts->range 1))
