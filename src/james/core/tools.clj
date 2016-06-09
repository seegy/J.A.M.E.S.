(ns james.core.tools)




(defn uuid
  "simple UUID creator"
  [] (str (java.util.UUID/randomUUID)))


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
