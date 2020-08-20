(ns hex.priority-set
  "Simple implementation of Priority Set for hex.core/path-finder"
  (:refer-clojure :exclude [peek pop!])
  #?(:clj (:import (clojure.lang Counted Seqable)
                   (java.lang Object)))
  #?(:clj (:gen-class)))

(defprotocol IPrioritySet
  "The setter is no longer use because it's an atom.
  data is sorted-map with priority as the key and value as set.
  Setp is value as the key and priority as value.
  If it is already there, remove it from the data and reinsert it."
  (get-data [_])
  (get-setp [_])
  (push! [this prio value]  "優先度付き push")
  (peek [this] "Topを見る")
  (pop! [this] "トップを取り出す"))

(defn- ps-push! [queue prio value]
  (let [data (get-data queue)
        setp (get-setp queue)]
    (when-let [old-prio (@setp value)]
      (swap! data #(update-in % [old-prio] disj value))
      (when (empty? (@data old-prio))
        (swap! data dissoc old-prio)))
    (if-let [cell (@data prio)]
      (swap! data assoc prio (conj cell value))
      (swap! data assoc prio #{value}))
    (swap! setp assoc value prio))
  queue)

(defn- ps-pop! [queue]
  (when-not (empty? @(get-setp queue))
    (let [db           (get-data queue)
          [prio items] (first @db)
          obj          (first items)
          rest-item    (rest items)]
      (if-not (empty? rest-item)
        (swap! db assoc prio rest-item)
        (swap! db dissoc prio))
      obj)))

#?(:cljs (deftype PrioritySet [data setp]

           IPrioritySet
           (get-data [_] data)
           (get-setp [_] setp)
           (push! [queue priority value] (ps-push! queue priority value))
           (peek [coll] (first (second (first @data))))
           (pop! [this] (ps-pop! this))

           ICounted
           (^number -count [this] (count @setp))

           ISeqable
           (-seq [this] (if (empty? @setp)
                          nil
                          (flatten (map #(seq (second %)) (seq @data))))))

   :clj (deftype PrioritySet [data setp]
          IPrioritySet
          (get-data [_] data)
          (get-setp [_] setp)
          (push! [queue priority value] (ps-push! queue priority value))
          (peek [coll] (first (second (first @data))))
          (pop! [this] (ps-pop! this))

          Counted
          (count [this] (count @setp))

          Seqable
          (seq [this] (if (empty? @setp)
                        nil
                        (flatten (map #(seq (second %)) (seq @data)))))
          Object
          (toString [this] (str @data))))

(defn priority-set-by
  "Create a priority set.
  Only the priority is changed if the same element is included in the push!."
  [comparator & priority-value]
  (let [queue (PrioritySet. (atom (sorted-map-by comparator)) (atom {}))]
    (loop [in (seq priority-value)
           out queue]
      (if in
        (recur (nnext in) (push! out (first in) (second in)))
        out))))

(defn priority-set
  "Creating a priority-set that gives priority to the lower priority"
  [& priority-value]
  (apply priority-set-by < priority-value))

