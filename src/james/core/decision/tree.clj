(ns james.core.decision.tree
  (:use james.core.datamodel.nodes)
  (:use james.core.tools))


(defn log2 [x]
  "Return the base-2 logarithm of x"
  (/ (Math/log x) (Math/log 2)))


(defn entropy [alist]
  "Given a list of pairs if (input, output), calculate the entropy of the list with respect to output"
  (let [n (count alist)
        counts (vals (frequencies (map #(nth % 1) alist)))]
    (reduce + (map #(* (- %) (log2 %)) (map #(/ % n) counts)))))


(defn partition-map
  "Given function and a seq, return a map of values the function can take when using members of the seq as an argument, to sub-seqs containing the parts of seq corresponding to those outputs"
  ([afn aseq]
   (partition-map {} afn aseq))
  ([acc afn aseq]
   (if (empty? aseq)
     acc
     (let [i (first aseq) o (afn i) inputs (get acc o)]
       (if inputs
         (recur
           (assoc acc o (conj inputs i)) afn (rest aseq))
         (recur
           (assoc acc o [i]) afn (rest aseq)))))))



(defn all-keys [aseq]
  "Given a list of maps, return a set containing all the keys from those maps"
  (into #{} (flatten2 (map keys aseq))))


(defn gain
  ([k alist]
   "Given a key and a list of pairs of (map, output), return the net reduction in entropy caused by partitioning the list according to the values associated with key in the input maps"
   (gain (entropy alist) k alist))
  ([current-entropy k alist]
   (let [parts (vals (partition-map #(k (first %)) alist))]
     (- current-entropy
        (reduce +
                (map #(* (entropy %) (/ (count %) (count alist))) parts))))))


(defn most-informative-key [aseq]
  "Given a sequence of pairs of (map, output), return the map key that provides the highest information gain about outout, when used to partition the sequence"
  (apply max-key #(gain % aseq) (all-keys (map first aseq))))


(defn map-vals
  ([amap fn]
   "Apply fn to each of the values in amap, and return a map associating those keys with the new values"
   (map-vals {} amap fn))
  ([acc amap fn]
   (if (empty? amap)
     acc
     (let [[k v] (first amap)]
       (recur (assoc acc k (fn v)) (rest amap) fn)))))

(defn build-decision-tree [aseq]
  "A decision tree is either: a pair consisting an input-map key, followed by a map of values to further trees, or a leaf node: a result (an output, in the language of the rest of this library). aseq should be a sequence of pairs of [input-map, output]. Returns nil if the seq is empty."
  (if (zero? (entropy aseq))
    (nth (first aseq) 1)
    (let [k (most-informative-key aseq)]
      [k (map-vals (partition-map #(k (first %)) aseq) build-decision-tree)])))


(defn tree-decide [tree input-map]
  "Given a decision tree and an input map, make decisions based on the contents of input-map and return the correct output (leaf node)."
  (if (vector? tree)
    (let [[k parts-map] tree]
      (tree-decide (parts-map (k input-map)) input-map))
    tree))


(defn comb [k l]
  (if (= 1 k) (map vector l)
      (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2))
                    (comb (dec k) (drop (inc %1) l)))
              l))))


(defn all-subsets [s]
  (apply concat
         (for [x (range 1 (inc (count s)))]
           (map #(into #{} %) (comb x s)))))

; (all-subsets #{1 2 3}) ;-> (#{1} #{3} #{2} #{1 3} #{1 2} #{3 2} #{1 3 2})

(defn all-submaps [m]
  (apply concat
         (for [x (range 1 (inc (count m)))]
           (map #(into {} %) (comb x m)))))

;(all-submaps {:a 1, :b 2, :c 3}) ; -> ({:a 1} {:b 2} {:c 3} {:b 2, :a 1} {:c 3, :a 1} {:c 3, :b 2} {:c 3, :b 2, :a 1})


(defn most-frequent-n [n items]
  (->> items
    frequencies
    (sort-by val)
    reverse
    (take n)
    (map first)))

;(most-frequent-n 2 [1 2 4 56 7 56 3 2 2 4 56 7 1 54 45 6 73 3 4 34 2 2 56]) ; -> (2 56)



(defn get-decision-for
  ([pname cat?]
  (let [tree (build-decision-tree (get-decision-data cat?))
        timemap (put-key-prefix (decorate-time {:date (now)}) :ordered)
        feed (merge timemap (get-last-order pname))]
    (println feed)
    (first (most-frequent-n 1 (filter #(not (nil? %))
                                      (map #(tree-decide tree %) (all-submaps feed)))))))
  ([pname]
   (get-decision-for pname true)))


; #####################################################################################################

(build-decision-tree (get-decision-data true))

(get-decision-for "Sören")
(get-decision-for "Silvia")
(get-decision-for "Heiko")





