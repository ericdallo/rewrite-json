(ns rewrite-json.zip
  "Zipper-based navigation and editing API for JSON/JSONC node trees.

   Navigation functions (right, left, down, up) skip whitespace and
   comment nodes by default, while preserving them in the tree.

   Usage:
     (require '[rewrite-json.zip :as z])
     (require '[rewrite-json.parser :as parser])

     (-> (z/of-string \"{\\\"name\\\": \\\"old\\\"}\")
         (z/find-key \"name\")
         z/right
         (z/replace (node/string-node \"new\"))
         z/root-string)
     ;; => \"{\\\"name\\\": \\\"new\\\"}\""
  (:refer-clojure :exclude [next replace remove])
  (:require
   [clojure.zip :as z]
   [rewrite-json.node :as node]
   [rewrite-json.parser :as parser]))

(defn ^:private json-branch? [node]
  (satisfies? node/InnerNode node))

(defn ^:private json-children [node]
  (node/children node))

(defn ^:private json-make-node [node children]
  (node/replace-children node (vec children)))

(defn of-node [root]
  (z/zipper json-branch? json-children json-make-node root))

(defn of-string [s]
  (of-node (parser/parse-string s)))

(defn ^:private skip-node? [loc]
  (let [n (z/node loc)]
    (contains? #{:whitespace :comment :comma :colon} (node/tag n))))

(defn right [loc]
  (loop [loc (z/right loc)]
    (cond
      (nil? loc) nil
      (skip-node? loc) (recur (z/right loc))
      :else loc)))

(defn left [loc]
  (loop [loc (z/left loc)]
    (cond
      (nil? loc) nil
      (skip-node? loc) (recur (z/left loc))
      :else loc)))

(defn down [loc]
  (when-let [loc (z/down loc)]
    (if (skip-node? loc)
      (right loc)
      loc)))

(defn up [loc]
  (z/up loc))

(defn rightmost [loc]
  (let [loc (z/rightmost loc)]
    (if (and loc (skip-node? loc))
      (left loc)
      loc)))

(defn leftmost [loc]
  (let [loc (z/leftmost loc)]
    (if (and loc (skip-node? loc))
      (right loc)
      loc)))

(defn node [loc]
  (z/node loc))

(defn value [loc]
  (node/value (z/node loc)))

(defn root [loc]
  (z/root loc))

(defn root-string [loc]
  (node/string (z/root loc)))

(defn find-key
  "From the current location (which should be an object or positioned
   inside one), find the entry whose key matches `key-name` (string).
   Returns the zipper positioned at the entry node, or nil.

   Note: clojure.zip/down on an empty children seq destructures to a nil
   node; we guard against that explicitly in the loop."
  [loc key-name]
  (let [obj-loc (if (identical? :object (node/tag (z/node loc)))
                  (z/down loc)
                  loc)]
    (loop [loc obj-loc]
      (when loc
        (let [n (z/node loc)]
          (when n  ;; clojure.zip/down on empty children yields nil node
            (if (and (identical? :entry (node/tag n))
                     (= key-name (node/value (node/key-node n))))
              loc
              (recur (z/right loc)))))))))

(defn find-value [loc v]
  (loop [loc loc]
    (when (and loc (not (z/end? loc)))
      (let [n (z/node loc)]
        (if (and (node/value-node? n) (= v (node/value n)))
          loc
          (recur (z/next loc)))))))

(defn replace [loc new-node]
  (z/replace loc new-node))

(defn edit [loc f & args]
  (z/replace loc (apply f (z/node loc) args)))

(defn ^:private remove-child-from-container [children target-idx]
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
          ;; Prefer left comma (handles middle/last elements cleanly)
          left-comma-idx
          [left-comma-idx (inc target-idx)]

          ;; Fallback to right comma (first element) — also consume whitespace after comma
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

(defn remove
  "Remove the node at the current location from its parent container.
   Handles comma cleanup for objects and arrays."
  [loc]
  (let [target (z/node loc)
        parent-loc (z/up loc)]
    (if parent-loc
      (let [parent (z/node parent-loc)
            children (vec (node/children parent))
            target-idx (first (keep-indexed
                               (fn [idx child] (when (identical? child target) idx))
                               children))
            new-children (remove-child-from-container children target-idx)]
        (z/replace parent-loc (node/replace-children parent new-children)))
      ;; At root — can't remove
      (throw (ex-info "Cannot remove root node" {})))))

(defn entry-value
  "Given a zipper positioned at an entry node, return a zipper positioned
   at the entry's value node (the first value-node after the colon)."
  [entry-loc]
  (when entry-loc
    (let [d (z/down entry-loc)]
      (loop [loc d
             past-colon? false]
        (when loc
          (let [n (z/node loc)]
            (cond
              (identical? :colon (node/tag n))
              (recur (z/right loc) true)

              (and past-colon? (node/value-node? n))
              loc

              :else
              (recur (z/right loc) past-colon?))))))))

(defn get-key [loc key-name]
  (when-let [entry-loc (find-key loc key-name)]
    (entry-value entry-loc)))

(defn ^:private infer-entry-style [obj-loc]
  (let [obj-node (z/node obj-loc)
        entries (filter node/entry? (node/children obj-node))]
    (if (seq entries)
      ;; Examine the first entry's internal structure to mimic it
      (let [first-entry (first entries)
            entry-children (node/children first-entry)
            ;; Space can be a separate whitespace node OR baked into the colon node (e.g. ": ")
            has-space-after-colon? (or (some #(and (node/whitespace? %)
                                                   (not (.contains ^String (node/string %) "\n")))
                                             entry-children)
                                       (some #(and (node/colon? %)
                                                   (.endsWith ^String (node/string %) " "))
                                             entry-children))]
        {:colon-sep (if has-space-after-colon? ": " ":")})
      {:colon-sep ": "})))

(defn ^:private infer-indent [container-node]
  (let [children (node/children container-node)
        ws-nodes (filter node/whitespace? children)
        ws-with-newlines (filter #(.contains ^String (node/string %) "\n") ws-nodes)]
    (if (seq ws-with-newlines)
      (let [ws-str ^String (node/string (first ws-with-newlines))
            ;; Text after the last newline = indent for entries inside this container
            after-nl (subs ws-str (inc (.lastIndexOf ws-str "\n")))
            ;; Closing indent = one level up = after-nl minus the last indent unit.
            ;; We detect the unit width by looking at the last 2 chars (tabs vs spaces).
            closing-indent (if (>= (count after-nl) 2)
                             (subs after-nl 0 (- (count after-nl) 2))
                             "")]
        {:newline true
         :indent after-nl
         :closing-indent closing-indent
         :ws-before (str "\n" after-nl)})
      {:newline false
       :indent ""
       :closing-indent ""
       :ws-before " "})))

(defn ^:private infer-indent-with-context
  "Like `infer-indent`, but when the container has no newline whitespace
   (e.g. it is a freshly created empty object), walks up ancestor zipper
   locs to find a multi-line container and derives the expected child-level
   indent for `container-loc` from it.

   Returns the same map shape as `infer-indent`."
  [container-loc]
  (let [direct (infer-indent (z/node container-loc))]
    (if (:newline direct)
      direct
      ;; Container has no whitespace — climb ancestors looking for one that does.
      ;; Only bump extra-levels for object/array ancestors; entry nodes are
      ;; transparent wrappers and don't add a visual indent level.
      (loop [loc (z/up container-loc)
             extra-levels 1]
        (if (nil? loc)
          direct  ;; reached root with no multi-line ancestor → stay single-line
          (let [ancestor-node   (z/node loc)
                ancestor-tag    (node/tag ancestor-node)
                ancestor-indent (infer-indent ancestor-node)]
            (if (:newline ancestor-indent)
              ;; Found a multi-line ancestor.  The indent for container-loc's
              ;; children is ancestor's child-indent + extra-levels more steps.
              (let [unit "  "
                    child-indent (str (:indent ancestor-indent)
                                      (apply str (repeat extra-levels unit)))
                    closing-indent (str (:indent ancestor-indent)
                                        (apply str (repeat (dec extra-levels) unit)))]
                {:newline true
                 :indent child-indent
                 :closing-indent closing-indent
                 :ws-before (str "\n" child-indent)})
              (recur (z/up loc)
                     (if (contains? #{:object :array} ancestor-tag)
                       (inc extra-levels)
                       extra-levels)))))))))

(defn ^:private find-last-idx [pred coll]
  (let [v (vec coll)]
    (loop [i (dec (count v))]
      (when (>= i 0)
        (if (pred (v i)) i (recur (dec i)))))))

(defn ^:private build-pretty-node
  "Recursively build a properly-indented node tree for `value-node`.

   Unlike `reindent-node` (which patched existing whitespace), this function
   constructs whitespace from scratch, so it works even when `value->node`
   produced a compact node with no internal whitespace.

   `child-indent`   – absolute indent string for entries/elements one level
                      inside this node (e.g. \"    \" for 4 spaces).
   `closing-indent` – absolute indent string for the closing `}` / `]`
                      (one level shallower than `child-indent`).

   Scalar nodes (string, number, boolean, null) are returned unchanged.
   Object and array nodes get their children replaced with a multi-line
   layout."
  [value-node child-indent closing-indent]
  (condp identical? (node/tag value-node)
    :object
    (let [entries (filter node/entry? (node/children value-node))
          next-child-indent  (str child-indent "  ")
          next-closing-indent child-indent
          new-entries
          (mapv (fn [entry]
                  (let [k (node/key-node entry)
                        v (node/value-of-entry entry)
                        pretty-v (build-pretty-node v next-child-indent next-closing-indent)]
                    ;; Use separate colon + whitespace nodes so infer-entry-style
                    ;; can detect the space and match it for subsequent appended entries.
                    (node/->EntryNode [k (node/->ColonNode ":") (node/whitespace-node " ") pretty-v])))
                entries)
          ;; Interleave: comma + \n+child-indent between entries, \n+closing-indent before }
          children-with-ws
          (if (seq new-entries)
            (-> []
                (into (mapcat (fn [[i e]]
                                (if (zero? i)
                                  [(node/whitespace-node (str "\n" child-indent)) e]
                                  [(node/->CommaNode ",") (node/whitespace-node (str "\n" child-indent)) e]))
                              (map-indexed vector new-entries)))
                (conj (node/whitespace-node (str "\n" closing-indent))))
            [])]
      (node/replace-children value-node children-with-ws))

    :array
    (let [elements (filter node/value-node? (node/children value-node))
          next-child-indent  (str child-indent "  ")
          next-closing-indent child-indent
          pretty-elements (mapv #(build-pretty-node % next-child-indent next-closing-indent)
                                elements)
          children-with-ws
          (if (seq pretty-elements)
            (-> []
                (into (mapcat (fn [[i e]]
                                (if (zero? i)
                                  [(node/whitespace-node (str "\n" child-indent)) e]
                                  [(node/->CommaNode ",") (node/whitespace-node (str "\n" child-indent)) e]))
                              (map-indexed vector pretty-elements)))
                (conj (node/whitespace-node (str "\n" closing-indent))))
            [])]
      (node/replace-children value-node children-with-ws))

    ;; Scalars — return as-is
    value-node))

(defn pretty-value-node
  "Convert `value-node` (already a node, typically from `node/value->node`)
   to a multi-line formatted node, using `container-loc` (a zipper loc
   positioned at the object/array that will contain the value) to infer
   the indentation level.

   Uses `infer-indent-with-context` so that even freshly created empty
   containers inherit multi-line style from their ancestors.

   When the container uses single-line formatting, or when `value-node` is
   a scalar, it is returned unchanged."
  [value-node container-loc]
  (let [indent-info (infer-indent-with-context container-loc)]
    (if (and (:newline indent-info)
             (contains? #{:object :array} (node/tag value-node)))
      (let [child-indent (str (:indent indent-info) "  ")
            closing-indent (:indent indent-info)]
        (build-pretty-node value-node child-indent closing-indent))
      value-node)))


(defn append-entry
  "Append a new key-value entry to an object.
   `loc` should be positioned at an object node.
   Tries to match the existing formatting style."
  [loc key-name value-node]
  (let [obj-node (z/node loc)
        children (vec (node/children obj-node))
        style (infer-entry-style loc)
        indent-info (infer-indent-with-context loc)
        key-n (node/string-node key-name)
        ;; Always produce separate colon + optional whitespace so infer-entry-style
        ;; can detect the space correctly on subsequent appends.
        entry-sep (if (= ": " (:colon-sep style))
                    [(node/->ColonNode ":") (node/whitespace-node " ")]
                    [(node/->ColonNode ":")])
        pretty-value (pretty-value-node value-node loc)
        new-entry (node/->EntryNode (vec (concat [key-n] entry-sep [pretty-value])))
        last-entry-idx (find-last-idx node/entry? children)
        new-children
        (if last-entry-idx
          (let [;; Split: [before-and-entry] [trailing-stuff]
                ;; trailing-stuff = whitespace/comma/comments between last entry and }
                after-last (subvec children (inc last-entry-idx))
                has-trailing-comma? (some node/comma? after-last)
                ;; Find closing whitespace (the whitespace before })
                closing-ws-idx (find-last-idx node/whitespace? children)
                has-closing-ws? (and closing-ws-idx (> closing-ws-idx last-entry-idx))
                ;; Build: everything up to last entry, comma, ws, new entry, closing ws
                before (subvec children 0 (inc last-entry-idx))
                separator (if has-trailing-comma?
                            []
                            [(node/->CommaNode ",")])
                ws-before-entry (if (:newline indent-info)
                                  [(node/whitespace-node (:ws-before indent-info))]
                                  [(node/whitespace-node " ")])
                closing (if has-closing-ws?
                          [(children closing-ws-idx)]
                          [])]
            (into [] (concat before separator ws-before-entry [new-entry] closing)))
          ;; No existing entries
          (if (:newline indent-info)
            [(node/whitespace-node (:ws-before indent-info))
             new-entry
             (node/whitespace-node (str "\n" (:closing-indent indent-info)))]
            [(node/whitespace-node " ")
             new-entry
             (node/whitespace-node " ")]))]
    (z/replace loc (node/replace-children obj-node new-children))))

(defn append-element [loc value-node]
  (let [arr-node (z/node loc)
        children (vec (node/children arr-node))
        indent-info (infer-indent-with-context loc)
        value-node (pretty-value-node value-node loc)
        last-val-idx (find-last-idx node/value-node? children)
        new-children
        (if last-val-idx
          (let [after-last (subvec children (inc last-val-idx))
                has-trailing-comma? (some node/comma? after-last)
                closing-ws-idx (find-last-idx node/whitespace? children)
                has-closing-ws? (and closing-ws-idx (> closing-ws-idx last-val-idx))
                before (subvec children 0 (inc last-val-idx))
                separator (if has-trailing-comma? [] [(node/->CommaNode ",")])
                ws-before-el (if (:newline indent-info)
                               [(node/whitespace-node (:ws-before indent-info))]
                               [(node/whitespace-node " ")])
                closing (if has-closing-ws?
                          [(children closing-ws-idx)]
                          [])]
            (into [] (concat before separator ws-before-el [value-node] closing)))
          [(node/whitespace-node (if (:newline indent-info) (:ws-before indent-info) ""))
           value-node])]
    (z/replace loc (node/replace-children arr-node new-children))))

(defn next [loc]
  (loop [loc (z/next loc)]
    (cond
      (z/end? loc) loc
      (skip-node? loc) (recur (z/next loc))
      :else loc)))

(defn end? [loc]
  (z/end? loc))