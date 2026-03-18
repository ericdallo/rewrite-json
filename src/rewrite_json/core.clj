(ns rewrite-json.core
  "High-level, path-based API for format-preserving JSON/JSONC editing.

   This namespace provides a simple `assoc-in` / `get-in` / `update-in`
   style interface that delegates to the zipper internally.

   Usage:
     (require '[rewrite-json.core :as rj])

     ;; Round-trip preserving formatting
     (-> (rj/parse-string json-str)
         (rj/assoc-in [\"name\"] \"new-value\")
         rj/to-string)

     ;; Read a nested value
     (rj/get-in (rj/parse-string json-str) [\"dependencies\" \"react\"])"
  (:refer-clojure :exclude [get-in assoc-in update-in])
  (:require
   [rewrite-json.node :as node]
   [rewrite-json.parser :as parser]
   [rewrite-json.zip :as z]))

(defn parse-string [s]
  (parser/parse-string s))

(defn to-string [root]
  (node/string root))

(defn ^:private navigate-path [loc path]
  (reduce
   (fn [loc segment]
     (when loc
       (cond
         ;; String key → navigate into object
         (string? segment)
         (z/get-key loc segment)

         ;; Integer index → navigate into array
         (integer? segment)
         (let [arr-loc (z/down loc)]
           (loop [loc arr-loc
                  idx 0]
             (when loc
               (if (= idx segment)
                 loc
                 (recur (z/right loc) (inc idx))))))

         :else
         (throw (ex-info (str "Invalid path segment: " (pr-str segment))
                         {:segment segment})))))
   (z/of-node loc)
   path))

(defn get-in
  "Get the Clojure value at `path` in the node tree.
   `path` is a vector of string keys and/or integer indices.

   Example: (get-in root [\"dependencies\" \"react\"])
            (get-in root [\"items\" 0 \"name\"])"
  ([root path]
   (get-in root path nil))
  ([root path not-found]
   (if-let [loc (navigate-path root path)]
     (z/value loc)
     not-found)))

(defn assoc-in
  "Set the value at `path` in the node tree.
   If the path exists, replaces the value node while preserving surrounding
   formatting. If the final key doesn't exist in an object, appends a new entry.
   `v` can be a Clojure value (string, number, boolean, nil, map, vector)
   or a node.

   Returns the updated root node."
  [root path v]
  (let [value-node (if (satisfies? node/Node v) v (node/value->node v))]
    (if (= 1 (count path))
      ;; Single-level path
      (let [segment (first path)
            loc (z/of-node root)]
        (if (string? segment)
          ;; Object key
          (if-let [val-loc (z/get-key loc segment)]
            (-> val-loc
                (z/replace value-node)
                z/root)
            ;; Key doesn't exist — append
            (-> loc
                (z/append-entry segment value-node)
                z/root))
          ;; Array index
          (if-let [target (navigate-path root path)]
            (-> target
                (z/replace value-node)
                z/root)
            (throw (ex-info (str "Array index out of bounds: " segment)
                            {:index segment})))))
      ;; Multi-level path
      (let [parent-path (vec (butlast path))
            last-segment (last path)]
        (if-let [parent-loc (navigate-path root parent-path)]
          (if (string? last-segment)
            ;; Navigate into parent object
            (let [parent-node (z/node parent-loc)]
              (if (identical? :object (node/tag parent-node))
                (if-let [val-loc (z/get-key parent-loc last-segment)]
                  (-> val-loc
                      (z/replace value-node)
                      z/root)
                  ;; Key doesn't exist in parent object — append entry
                  (-> parent-loc
                      (z/append-entry last-segment value-node)
                      z/root))
                (throw (ex-info (str "Expected object at path " (pr-str parent-path)
                                     ", got " (node/tag parent-node))
                                {:path parent-path}))))
            ;; Array index in parent
            (if-let [target (navigate-path root path)]
              (-> target
                  (z/replace value-node)
                  z/root)
              (throw (ex-info (str "Array index out of bounds: " last-segment)
                              {:index last-segment}))))
          (throw (ex-info (str "Path not found: " (pr-str parent-path))
                          {:path parent-path})))))))

(defn update-in [root path f & args]
  (let [current (get-in root path)]
    (assoc-in root path (apply f current args))))

(defn ^:private remove-child-at [children target-idx]
  (let [v (vec children)
        n (count v)
        right-comma-idx (loop [i (inc target-idx)]
                          (when (< i n)
                            (let [child (v i)]
                              (cond
                                (node/comma? child) i
                                (node/whitespace? child) (recur (inc i))
                                :else nil))))
        left-comma-idx (loop [i (dec target-idx)]
                         (when (>= i 0)
                           (let [child (v i)]
                             (cond
                               (node/comma? child) i
                               (node/whitespace? child) (recur (dec i))
                               :else nil))))
        [remove-start remove-end]
        (cond
          left-comma-idx
          [left-comma-idx (inc target-idx)]

          right-comma-idx
          (let [end (inc right-comma-idx)
                end (loop [i end]
                      (if (and (< i n) (node/whitespace? (v i)))
                        (recur (inc i))
                        i))]
            [target-idx end])

          :else
          [target-idx (inc target-idx)])]
    (into (subvec v 0 remove-start)
          (subvec v remove-end))))

(defn ^:private find-target-idx [children segment]
  (cond
    (string? segment)
    (first (keep-indexed
            (fn [idx child]
              (when (and (node/entry? child)
                         (= segment (node/value (node/key-node child))))
                idx))
            children))

    (integer? segment)
    (let [value-indices (keep-indexed
                         (fn [idx child]
                           (when (node/value-node? child) idx))
                         children)]
      (nth (vec value-indices) segment nil))))

(defn dissoc-in [root path]
  (let [parent-path (vec (butlast path))
        last-segment (last path)]
    (if (empty? parent-path)
      (let [children (vec (node/children root))
            target-idx (find-target-idx children last-segment)]
        (if target-idx
          (node/replace-children root (remove-child-at children target-idx))
          root))
      (if-let [parent-loc (navigate-path root parent-path)]
        (let [container (z/node parent-loc)
              children (vec (node/children container))
              target-idx (find-target-idx children last-segment)]
          (if target-idx
            (-> parent-loc
                (z/replace (node/replace-children container
                                                  (remove-child-at children target-idx)))
                z/root)
            (z/root parent-loc)))
        root))))
