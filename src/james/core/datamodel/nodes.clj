(ns james.core.datamodel.nodes
  (:use james.core.tools)
  (:use loom.graph loom.attr loom.io loom.derived loom.label loom.alg)
  (:use clojure.java.io)
  (:require [clojure.edn    :as edn]))


(def vertex-types #{:default, :person, :order, :deliverer, :rating, :picture, :order-pos, :food})

(def edge-types #{:created :default, :ordered, :comesFrom, :rates, :lookslike, :hasRating})

(def file-path "/opt/james/brain.graph")


(defn date-properties
  [date-ts]
  {:date date-ts})


(defn new-vertex
  [g id vertex-name vertex-type properties]
  (let [ id (if (= id "")
              (uuid)
              id)
         vertex-type (if (contains? vertex-types vertex-type)
                       vertex-type
                       :default)
         properties (if (map? properties)
                      properties
                      (if (empty? properties)
                        {}
                        {:default properties}))
         g (add-nodes g id)]
    (reduce (fn [g [k v]] (add-attr g id k v)) g (merge (date-properties (now))
                                                        {:label (str vertex-name " " id), :name vertex-name, :type vertex-type}
                                                        properties))))


(defn new-edge
  ([g edge-type source target ]
   (new-edge g edge-type source target {}))
  ([g edge-type source target properties]
   (let [ edge-type (if (contains? edge-types edge-type)
                      edge-type
                      :default)
          properties (if (map? properties)
                       properties
                       (if (empty? properties)
                         {}
                         {:default properties}))
          g (add-edges g [source target])]
     (reduce (fn [g [k v]] (add-attr-to-edges g k v [[source target]] )) g (merge (date-properties (now))
                                                                                  {:label edge-type
                                                                                   :type edge-type}
                                                                                  properties)))))



(defn find-vertices
  [g attr]
  (let [graph-attrs (:attrs g)
        ks (keys attr)]
    (keys (filter (fn [[k v]] (= attr (select-keys v ks))) graph-attrs))))


(defn find-edge
  [g n1 n2 attr]
  )

(defn save-memory
  [graph]
  (spit file-path  (prn-str graph)))


(defn create-default-graph
  []
  (-> (graph)
      (new-vertex "1" "Sören" :person {:role "admin" :birth "13-09-1989"})))


(defn load-existing-memory
  []
  (if (.exists (as-file file-path))
    (edn/read-string {:readers {'loom.graph.BasicEditableGraph loom.graph/map->BasicEditableGraph}} (slurp file-path))
    (create-default-graph)))


(def james-graph (ref (load-existing-memory)))


(defn add-person
  [name ops creator-id?]
  (let [creator-id (if (nil? creator-id?)
                     "1"
                     creator-id?)
        id (uuid)]
    (dosync
      (ref-set james-graph  (new-vertex @james-graph id name :person ops))
      (save-memory @james-graph)
      id)))


(defn add-deliverer
  [name ops]
  (let [id (uuid)]
    (dosync
      (ref-set james-graph (new-vertex @james-graph id name :deliverer ops))
      (save-memory @james-graph))
    id))



(defn add-food
  [foodname deliverer-id ops ]
  (let [id (uuid)]
    (dosync
      (ref-set james-graph (new-edge (new-vertex @james-graph id foodname :food ops)
                                     :comesFrom id deliverer-id {}))
      (save-memory @james-graph))
    id))


(defn add-order
  [person-id food-id n date?]
  (let [date (if (nil? date?)
               (now)
               date?)]
    (dosync
      (ref-set james-graph (new-edge @james-graph :ordered person-id food-id (merge {:count n} (date-properties date))))
      (save-memory @james-graph))
    [person-id food-id]))



(defn add-rating
  [person-id food-id rating ops]
  (let [id (uuid)]
    (dosync
      (ref-set james-graph (new-edge (new-edge (new-vertex @james-graph id (str rating "*") :rating ops)
                                               :hasRating food-id id {})
                                     :rates person-id id {}))
      (save-memory @james-graph)
      id)))


(defn add-order-with-rating
  [person-id food-id n date? rating note?]
  (let [ordering (add-order person-id food-id n date?)
        ]
    (add-rating person-id food-id rating {:note note?})))


(defn find-vertex
  [g name type]
  (let [results (find-vertices g {:type type, :name name})]
    (if (not (empty? results))
      (first results)
      nil)))



(defn find-or-create-person
  [name ops]
  (let [result (find-vertex @james-graph name :person)]
    (if (nil? result)
      (add-person name ops nil)
      result)))


(defn find-or-create-deliverer
  [name ops]
  (let [result (find-vertex @james-graph  name :deliverer)]
    (if (nil? result)
      (add-deliverer name ops)
      result)))


(defn- get-edges-by-type
  [g node type]
  (sequence  (comp
               (filter (fn[edge] (contains? (set edge) node)))
               (filter (fn[edge] (= type (attr @james-graph (first edge) (second edge) :type)))))
             (distinct-edges (subgraph-reachable-from @james-graph node))))

(defn- put-key-prefix
  [attrs prefix]
  (let [ keys-of-atts (keys attrs)
         replacement (into {} (map (fn[k] [k (->> k name (str (name prefix) "-") keyword)]) keys-of-atts ))]
    (clojure.set/rename-keys attrs replacement)))



(defn- distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x)]
            (if (contains? @seen fx)
              result
              (do (vswap! seen conj fx)
                (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x)]
                         (if (contains? seen fx)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen fx)))))))
                   xs seen)))]
     (step coll #{}))))

(defn get-decision-data
  [cat?]
  (let [g @james-graph
        people (find-vertices g {:type :person})
        people-and-orders (map (fn [p] [p (into #{} (remove #(= % p ) (flatten (get-edges-by-type g p :ordered))))]) people)
        raw-orders (flatten (map (fn [[pid orders ]] (map (fn[foodid]
                                                            (let [ get-target-id-of-edge (fn [source edgetype] (first (remove #(= % source ) (first (get-edges-by-type g source edgetype)))))
                                                                   rating (get-target-id-of-edge foodid :hasRating )
                                                                   deliverer (get-target-id-of-edge foodid :comesFrom )
                                                                   get-prefixed-attrs (fn[id prefix nogos] (if (nil? id)
                                                                                                             {}
                                                                                                             (put-key-prefix
                                                                                                               (apply dissoc (attrs g id) :label :type nogos)
                                                                                                               prefix)))]
                                                              (merge (get-prefixed-attrs pid :person #{:date})
                                                                     (decorate-time (assoc (get-prefixed-attrs foodid :food #{}) :food-date (:date (attrs g pid foodid))))
                                                                     (get-prefixed-attrs rating :rating #{:date :note})
                                                                     (get-prefixed-attrs deliverer :deliverer #{:date})
                                                                     ))) orders)) people-and-orders))
        orders-by-peoples (partition-by :person-name raw-orders)
        ]
    (into [] (apply concat (map (fn [list-of-orders] (let [sorted-list (sort-by :food-date (distinct-by :food-date list-of-orders))]
                                         (mapv (fn[a b] [ (dissoc
                                                         (assoc a
                                                           :further-food-cat (:food-category b)
                                                           :further-food (:food-name b)
                                                           :further-rating (:rating-name b)
                                                           :further-deliverer (:deliverer-name b)) :food-category :person-birth :person-role :food-name :rating-name :deliverer-name :food-date )
                                                         (select-keys a (if cat? [:food-category :deliverer-name] [:food-name :deliverer-name]))])
                                              sorted-list (conj sorted-list {})))) orders-by-peoples)))))





(defn get-last-order
  [pname]
  (let [g @james-graph
         pid (first (find-vertices g {:type :person :name pname}))
        orders (map (fn [[_ oid]] [(attrs g  oid)
                                   (attrs g pid oid)
                                   (attrs g (last (last (get-edges-by-type g oid :comesFrom))))
                                   (attrs g (last (last (get-edges-by-type g oid :hasRating))))]) (get-edges-by-type g pid :ordered))
       [lastfood lastorder lastdeliverer lastrating] (apply max-key #(:date (second %)) orders)]
  {:person-name pname
   :further-food-cat (:category lastfood)
   :further-deliverer (:name lastdeliverer)
   :further-food (:name lastfood)
   :further-rating (:name lastrating)}))

; ############################################

(def traindata (get-decision-data false))

(doseq  [s traindata]
  (println s))


; Some default data

#_(
    (add-order-with-rating "1"
                          (add-food "Schnitzel Napoli"
                                    (find-or-create-deliverer "Schnitzelhaus" {}) {:category "Schnitzel"})
                          1
                          1459182354000 3 "")

    (add-order-with-rating (find-or-create-person "Silvia" {})
                          (add-food "Chicken Wrap"
                                    (find-or-create-deliverer "Schnitzelhaus" {}) {:category "Wrap"})
                          1
                          1459182354000 4 "")

    (add-order-with-rating "1"
                          (add-food "Drehspieß Wrap"
                                    (find-or-create-deliverer "Schnitzelhaus" {}) {:category "Wrap"})
                          1
                          1459182354000 2 "")

    (add-order-with-rating "1"
                          (add-food "Gyros Teller"
                                    (find-or-create-deliverer "Vulkan" {}) {:category "Griechisch"})
                          1
                          1460929074000 3 "")

    (add-order-with-rating "1"
                          (add-food "Pizza Tonno"
                                    (find-or-create-deliverer "Himalaya" {}) {:category "Pizza"})
                          1
                          1459282374000 4 "")

      (add-order-with-rating "1"
                          (add-food "Pizza Tonno"
                                    (find-or-create-deliverer "Himalaya" {}) {:category "Pizza"})
                          1
                          1461510294000 4 "")

   (add-order-with-rating (find-or-create-person "Silvia" {})
                          (add-food "Pizza Quattro Stagioni"
                                    (find-or-create-deliverer "Himalaya" {}) {:category "Pizza"})
                          1
                          1461510294000 4 "")

    (add-order-with-rating "1"
                          (add-food "Hawaii-Schnitzel"
                                    (find-or-create-deliverer "Schnitzelhaus" {}) {:category "Schnitzel"})
                          1
                          1462478874000 4 "")

    (add-order-with-rating (find-or-create-person "Silvia" {})
                          (add-food "Pasta Classico"
                                    (find-or-create-deliverer "Schnitzelhaus" {}) {:category "Pasta"})
                          1
                          1462478874000 2 "")

   (add-order-with-rating "1"
                          (add-food "Pizza-Brötchen"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Beilage"})
                          1
                          1464192331000 5 "")

   (add-order-with-rating "1"
                          (add-food "Mam Burger"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Burger"})
                          1
                          1464192331000 3 "")

   (add-order-with-rating (find-or-create-person "Heiko" {})
                          (add-food "Pizza-Brötchen"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Beilage"})
                          1
                          1464192331000 4 "")


   (add-order-with-rating (find-or-create-person "Heiko" {})
                          (add-food "Pizza"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Pizza"})
                          1
                          1464192331000 4 "")

   (add-order-with-rating "1"
                          (add-food "Ente süß-sauer"
                                    (find-or-create-deliverer "China Boy" {}) {:category "Chinesisch"})
                          1
                          1464283100347 2 "")

   (add-order-with-rating "1"
                          (add-food "Pizza-Brötchen"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Beilage"})
                          1
                          1458492283000 4 "")

   (add-order-with-rating "1"
                          (add-food "Pizza Momento"
                                    (find-or-create-deliverer "Napoli" {}) {:category "Pizza"})
                          1
                          1458492283000 3 "")

   (add-order-with-rating "1"
                          (add-food "Pizza Hawaii"
                                    (find-or-create-deliverer "Himalaya" {}) {:category "Pizza"})
                          1
                          1464552710045 3 "")

   (add-order-with-rating (find-or-create-person "Silvia" {})
                          (add-food "Pizza Tonno"
                                    (find-or-create-deliverer "Himalaya" {}) {:category "Pizza"})
                          1
                          1464552710045 3 "")

  (add-order-with-rating (find-or-create-person "Heiko" {})
                          (add-food "Enchilada"
                                    (find-or-create-deliverer "Vulkan" {}) {:category "Wrap"})
                          1
                          1465334571000 5 "")
   )


;(nodes-filtered-by #(= "Sören" (:name %)) data-graph)
 ;(view @james-graph)
