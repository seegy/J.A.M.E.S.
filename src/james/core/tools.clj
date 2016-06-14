(ns james.core.tools)




(defn uuid
  "simple UUID creator"
  [] (str (java.util.UUID/randomUUID)))

(defn flatten2
  "Like `clojure.core/flatten` but better, stronger, faster.
  Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat, lazy sequence.
  If the argument is non-sequential (numbers, maps, strings, nil,
  etc.), returns the original argument."
  {:static true}
  [x]
  (letfn [(flat [coll]
                  (lazy-seq
                   (when-let [c (seq coll)]
                     (let [x (first c)]
                       (if (sequential? x)
                         (concat (flat x) (flat (rest c)))
                         (cons x (flat (rest c))))))))]
    (if (sequential? x) (flat x) x)))


; ################ TIME STAFF

(require '[clj-time.core :as time]
         '[clj-time.coerce :as tc])

(defn now
  ""
  [] (tc/to-long (time/now)))

(defn month-by-ts
  [ts]
  (time/month (tc/from-long ts)))

(defn day-by-ts
  [ts]
  (time/day (tc/from-long ts)))

(defn weekday-by-ts
  [ts]
  (time/day-of-week (tc/from-long ts)))

(defn hour-by-ts
  [ts]
  (time/hour (tc/from-long ts)))

(defn decorate-time
  [m]
  (let [date-ts (:date m)]
    (merge m {:day-of-week (weekday-by-ts date-ts), :day-of-month (day-by-ts date-ts), :month  (month-by-ts date-ts), :hour (hour-by-ts date-ts)})))
