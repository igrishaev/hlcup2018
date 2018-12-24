(ns hlcup.db.func)


(defn diff
  [^Number a ^Number b]
  (let [c (- a b)]
    (cond
      (neg? c) (- c)
      :else c)))


(comment
  (dist 100 105)
  (dist 133 200))
