(ns james.core
  (:gen-class))

(def my-name "J.A.M.E.S.")

(defn- sayHello
  []
  (println "Hello Sir! I am " my-name ". How can I help you"))

(defn -main
  [& args]
  (sayHello))



