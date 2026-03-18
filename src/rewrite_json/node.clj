(ns rewrite-json.node
  "Node protocols, types, constructors, and predicates for
   format-preserving JSON/JSONC representation.")

(defprotocol Node
  "A single node in the JSON tree.  Every node can reproduce the exact
   source text it was parsed from and carries a keyword tag."
  (tag [node] "Keyword identifying the node type.")
  (string [node] "Reproduce the original source text for this node.")
  (value [node] "Return a plain Clojure value (string, number, bool, nil,
                  map, vector) or nil for non-value nodes like whitespace."))

(defprotocol InnerNode
  "Extended protocol for nodes that contain children."
  (children [node] "Return the seq of child nodes.")
  (replace-children [node new-children] "Return a new node with replaced children."))

(defrecord StringNode [raw val]
  Node
  (tag [_] :string)
  (string [_] raw)
  (value [_] val))

(defrecord NumberNode [raw val]
  Node
  (tag [_] :number)
  (string [_] raw)
  (value [_] val))

(defrecord BooleanNode [raw val]
  Node
  (tag [_] :boolean)
  (string [_] raw)
  (value [_] val))

(defrecord NullNode [raw]
  Node
  (tag [_] :null)
  (string [_] raw)
  (value [_] nil))

(defrecord WhitespaceNode [raw]
  Node
  (tag [_] :whitespace)
  (string [_] raw)
  (value [_] nil))

(defrecord CommentNode [raw style]
  Node
  (tag [_] :comment)
  (string [_] raw)
  (value [_] nil))

(defrecord CommaNode [raw]
  Node
  (tag [_] :comma)
  (string [_] raw)
  (value [_] nil))

(defrecord ColonNode [raw]
  Node
  (tag [_] :colon)
  (string [_] raw)
  (value [_] nil))

(defrecord EntryNode [children*]
  Node
  (tag [_] :entry)
  (string [_] (apply str (map string children*)))
  (value [this]
    (let [meaningful (remove #(#{:whitespace :comment :colon} (tag %)) (children this))]
      (when (= 2 (count meaningful))
        (let [[k v] meaningful]
          [(value k) (value v)]))))
  InnerNode
  (children [_] children*)
  (replace-children [node new-children] (assoc node :children* new-children)))

(defrecord ArrayNode [children*]
  Node
  (tag [_] :array)
  (string [this]
    (str "[" (apply str (map string (children this))) "]"))
  (value [this]
    (let [meaningful (remove #(#{:whitespace :comment :comma} (tag %))
                             (children this))]
      (mapv value meaningful)))
  InnerNode
  (children [_] children*)
  (replace-children [node new-children] (assoc node :children* new-children)))

(defrecord ObjectNode [children*]
  Node
  (tag [_] :object)
  (string [this]
    (str "{" (apply str (map string (children this))) "}"))
  (value [this]
    (let [entries (filter #(identical? :entry (tag %)) (children this))]
      (into {}
            (map (fn [entry]
                   (let [[k v] (value entry)]
                     [k v]))
                 entries))))
  InnerNode
  (children [_] children*)
  (replace-children [node new-children] (assoc node :children* new-children)))

(defn ^:private escape-json-string ^String [^String s]
  (let [sb (StringBuilder. (+ 2 (count s)))]
    (.append sb \")
    (dotimes [i (.length s)]
      (let [ch (.charAt s i)]
        (case ch
          \\       (.append sb "\\\\")
          \"       (.append sb "\\\"")
          \newline (.append sb "\\n")
          \return  (.append sb "\\r")
          \tab     (.append sb "\\t")
          \backspace (.append sb "\\b")
          \formfeed  (.append sb "\\f")
          (if (< (int ch) 0x20)
            (do (.append sb "\\u")
                (.append sb (format "%04x" (int ch))))
            (.append sb ch)))))
    (.append sb \")
    (.toString sb)))

(defn string-node [s]
  (->StringNode (escape-json-string s) s))

(defn number-node [n]
  (->NumberNode (str n) n))

(defn boolean-node [b]
  (->BooleanNode (str b) b))

(defn null-node []
  (->NullNode "null"))

(defn whitespace-node [ws]
  (->WhitespaceNode ws))

(defn comment-node [raw style]
  (->CommentNode raw style))

(defn comma-node []
  (->CommaNode ","))

(defn colon-node []
  (->ColonNode ":"))

(defn entry-node
  "Create an object entry node from a key node, value node,
   and optional whitespace/colon nodes between them.
   If only key and value are provided, a default `: ` separator is used."
  ([key-node value-node]
   (->EntryNode [key-node (->ColonNode ": ") value-node]))
  ([child-nodes]
   (->EntryNode (vec child-nodes))))

(defn array-node [children]
  (->ArrayNode (vec children)))

(defn object-node [children]
  (->ObjectNode (vec children)))

(declare value->node)

(defn ^:private pairs->entry-children [pairs]
  (let [entries (map (fn [[k v]]
                       (entry-node (string-node (name k))
                                   (value->node v)))
                     pairs)]
    (interpose (->CommaNode ", ") entries)))

(defn value->node
  "Convert a plain Clojure value to the corresponding node tree."
  [x]
  (cond
    (nil? x)    (null-node)
    (string? x) (string-node x)
    (number? x) (number-node x)
    (boolean? x) (boolean-node x)
    (map? x)    (object-node (vec (pairs->entry-children x)))
    (vector? x) (array-node (vec (interpose (->CommaNode ", ")
                                            (map value->node x))))
    (sequential? x) (array-node (vec (interpose (->CommaNode ", ")
                                                (map value->node x))))
    :else (throw (ex-info (str "Cannot convert value to JSON node: " (pr-str x))
                          {:value x}))))

(defn whitespace? [node]
  (identical? :whitespace (tag node)))

(defn comment? [node]
  (identical? :comment (tag node)))

(defn comma? [node]
  (identical? :comma (tag node)))

(defn colon? [node]
  (identical? :colon (tag node)))

(defn entry? [node]
  (identical? :entry (tag node)))

(defn value-node? [node]
  (contains? #{:string :number :boolean :null :object :array} (tag node)))

(defn container? [node]
  (contains? #{:object :array :entry} (tag node)))

(defn printable? [node]
  (not (contains? #{:whitespace :comment} (tag node))))

(defn key-node [entry]
  (first (filter #(identical? :string (tag %)) (children entry))))

(defn value-of-entry [entry]
  (last (filter value-node? (children entry))))
