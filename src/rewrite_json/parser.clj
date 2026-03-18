(ns rewrite-json.parser
  "Format-preserving recursive-descent parser for JSON and JSONC.

   Reads every character of the source into a node tree so the original
   text can be reproduced exactly via `(node/string root)`.

   Supports:
   - Standard JSON (RFC 8259)
   - JSONC extensions: line comments (//), block comments (/* */), trailing commas"
  (:require
   [rewrite-json.node :as node])
  (:import
   [java.io PushbackReader StringReader]))

(set! *warn-on-reflection* true)

(defn ^:private make-reader ^PushbackReader [^String s]
  (PushbackReader. (StringReader. s) 2))

(defn ^:private read-char ^long [^PushbackReader reader]
  (.read reader))

(defn ^:private peek-char ^long [^PushbackReader reader]
  (let [ch (.read reader)]
    (when (>= ch 0)
      (.unread reader ch))
    ch))

(defn ^:private unread-char [^PushbackReader reader ch]
  (when (>= (long ch) 0)
    (.unread reader (int ch))))

(defn ^:private whitespace-char? [^long ch]
  (or (= ch (long \space))
      (= ch (long \tab))
      (= ch (long \newline))
      (= ch (long \return))))

(defn ^:private parse-whitespace [^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-char reader)]
        (if (and (>= ch 0) (whitespace-char? ch))
          (do (.append sb (char ch))
              (recur))
          (do (unread-char reader ch)
              (when (pos? (.length sb))
                (node/->WhitespaceNode (.toString sb)))))))))

(defn ^:private parse-line-comment [^PushbackReader reader]
  ;; Already consumed first '/'; consume second '/'
  (let [ch2 (read-char reader)]
    (when (= ch2 (long \/))
      (let [sb (StringBuilder. "//")]
        (loop []
          (let [ch (read-char reader)]
            (cond
              (< ch 0)
              (node/->CommentNode (.toString sb) :line)

              (= ch (long \newline))
              (do (unread-char reader ch)
                  (node/->CommentNode (.toString sb) :line))

              :else
              (do (.append sb (char ch))
                  (recur)))))))))

(defn ^:private parse-block-comment [^PushbackReader reader]
  ;; Already consumed '/'; consume '*'
  (let [ch2 (read-char reader)]
    (when (= ch2 (long \*))
      (let [sb (StringBuilder. "/*")]
        (loop []
          (let [ch (read-char reader)]
            (cond
              (< ch 0)
              (node/->CommentNode (.toString sb) :block)

              (and (= ch (long \*))
                   (= (peek-char reader) (long \/)))
              (do (.append sb "*/")
                  (read-char reader) ;; consume '/'
                  (node/->CommentNode (.toString sb) :block))

              :else
              (do (.append sb (char ch))
                  (recur)))))))))

(defn ^:private parse-comment [^PushbackReader reader]
  ;; Already consumed first '/'
  (let [next-ch (peek-char reader)]
    (cond
      (= next-ch (long \/)) (parse-line-comment reader)
      (= next-ch (long \*)) (parse-block-comment reader)
      :else (throw (ex-info "Unexpected character after '/'" {})))))

(defn ^:private collect-extras [^PushbackReader reader]
  (loop [nodes []]
    (let [ch (peek-char reader)]
      (cond
        (and (>= ch 0) (whitespace-char? ch))
        (recur (conj nodes (parse-whitespace reader)))

        (= ch (long \/))
        (do (read-char reader) ;; consume '/'
            (recur (conj nodes (parse-comment reader))))

        :else
        nodes))))

(declare parse-value)

(defn ^:private read-unicode-escape [^PushbackReader reader ^StringBuilder raw-sb]
  (let [hex-sb (StringBuilder. 4)]
    (dotimes [_ 4]
      (let [ch (read-char reader)]
        (when (< ch 0)
          (throw (ex-info "Unterminated unicode escape" {})))
        (.append raw-sb (char ch))
        (.append hex-sb (char ch))))
    (char (Integer/parseInt (.toString hex-sb) 16))))

(defn ^:private decode-escape-char [^long ch]
  (case ch
    34  \"      ;; \"
    92  \\      ;; \\
    47  \/      ;; \/
    110 \newline ;; \n
    114 \return  ;; \r
    116 \tab     ;; \t
    98  \backspace ;; \b
    102 \formfeed  ;; \f
    (throw (ex-info (str "Invalid escape character: \\" (char ch)) {:char (char ch)}))))

(defn ^:private parse-json-string [^PushbackReader reader]
  (let [raw-sb (StringBuilder. "\"")
        val-sb (StringBuilder.)]
    (read-char reader) ;; consume opening '"'
    (loop []
      (let [ch (read-char reader)]
        (cond
          (< ch 0)
          (throw (ex-info "Unterminated string" {}))

          (= ch (long \\))
          (let [escaped (read-char reader)]
            (when (< escaped 0)
              (throw (ex-info "Unterminated string escape" {})))
            (.append raw-sb (char ch))
            (.append raw-sb (char escaped))
            (if (= escaped (long \u))
              (let [decoded (read-unicode-escape reader raw-sb)]
                (.append val-sb decoded))
              (.append val-sb (decode-escape-char escaped)))
            (recur))

          (= ch (long \"))
          (do (.append raw-sb \")
              (node/->StringNode (.toString raw-sb) (.toString val-sb)))

          :else
          (do (.append raw-sb (char ch))
              (.append val-sb (char ch))
              (recur)))))))

(defn ^:private parse-number [^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-char reader)]
        (if (and (>= ch 0)
                 (or (Character/isDigit (char ch))
                     (= ch (long \.))
                     (= ch (long \-))
                     (= ch (long \+))
                     (= ch (long \e))
                     (= ch (long \E))))
          (do (.append sb (char ch))
              (recur))
          (do (unread-char reader ch)
              (let [raw (.toString sb)
                    val (try
                          (if (or (.contains raw ".")
                                  (.contains raw "e")
                                  (.contains raw "E"))
                            (Double/parseDouble raw)
                            (Long/parseLong raw))
                          (catch NumberFormatException _
                            (try
                              (bigdec raw)
                              (catch NumberFormatException _
                                (throw (ex-info (str "Invalid number: " raw) {:raw raw}))))))]
                (node/->NumberNode raw val))))))))

(defn ^:private parse-keyword-token [^PushbackReader reader ^String expected]
  (let [sb (StringBuilder.)]
    (dotimes [_ (count expected)]
      (let [ch (read-char reader)]
        (when (< ch 0)
          (throw (ex-info (str "Unexpected end of input, expected '" expected "'") {})))
        (.append sb (char ch))))
    (let [raw (.toString sb)]
      (when-not (= raw expected)
        (throw (ex-info (str "Expected '" expected "', got '" raw "'") {})))
      raw)))

(defn ^:private parse-true [^PushbackReader reader]
  (let [raw (parse-keyword-token reader "true")]
    (node/->BooleanNode raw true)))

(defn ^:private parse-false [^PushbackReader reader]
  (let [raw (parse-keyword-token reader "false")]
    (node/->BooleanNode raw false)))

(defn ^:private parse-null [^PushbackReader reader]
  (let [raw (parse-keyword-token reader "null")]
    (node/->NullNode raw)))

(defn ^:private parse-array [^PushbackReader reader]
  (read-char reader) ;; consume '['
  (loop [children []]
    (let [extras (collect-extras reader)
          children (into children extras)
          ch (peek-char reader)]
      (cond
        ;; end of array
        (= ch (long \]))
        (do (read-char reader)
            (node/->ArrayNode children))

        ;; comma (between elements or trailing)
        (= ch (long \,))
        (do (read-char reader)
            (recur (conj children (node/->CommaNode ","))))

        ;; value
        (>= ch 0)
        (let [val-node (parse-value reader)]
          (recur (conj children val-node)))

        :else
        (throw (ex-info "Unterminated array" {}))))))

(defn ^:private parse-entry [^PushbackReader reader key-node]
  ;; key-node already parsed; now read colon + value with intervening extras
  (let [pre-colon (collect-extras reader)
        colon-ch (read-char reader)
        _ (when-not (= colon-ch (long \:))
            (throw (ex-info (str "Expected ':' in object entry, got '"
                                 (if (< colon-ch 0) "EOF" (str (char colon-ch))) "'")
                            {})))
        colon (node/->ColonNode ":")
        post-colon (collect-extras reader)
        val-node (parse-value reader)
        entry-children (-> []
                          (conj key-node)
                          (into pre-colon)
                          (conj colon)
                          (into post-colon)
                          (conj val-node))]
    (node/->EntryNode entry-children)))

(defn ^:private parse-object [^PushbackReader reader]
  (read-char reader) ;; consume '{'
  (loop [children []]
    (let [extras (collect-extras reader)
          children (into children extras)
          ch (peek-char reader)]
      (cond
        ;; end of object
        (= ch (long \}))
        (do (read-char reader)
            (node/->ObjectNode children))

        ;; comma (between entries or trailing)
        (= ch (long \,))
        (do (read-char reader)
            (recur (conj children (node/->CommaNode ","))))

        ;; string key -> entry
        (= ch (long \"))
        (let [key-node (parse-json-string reader)
              entry (parse-entry reader key-node)]
          (recur (conj children entry)))

        (< ch 0)
        (throw (ex-info "Unterminated object" {}))

        :else
        (throw (ex-info (str "Unexpected character in object: " (char ch))
                        {:char (char ch)}))))))

(defn ^:private parse-value [^PushbackReader reader]
  (let [ch (peek-char reader)]
    (cond
      (< ch 0) (throw (ex-info "Unexpected end of input" {}))
      (= ch (long \")) (parse-json-string reader)
      (= ch (long \{)) (parse-object reader)
      (= ch (long \[)) (parse-array reader)
      (= ch (long \t)) (parse-true reader)
      (= ch (long \f)) (parse-false reader)
      (= ch (long \n)) (parse-null reader)
      (or (= ch (long \-))
          (Character/isDigit (char ch))) (parse-number reader)
      :else (throw (ex-info (str "Unexpected character: " (char ch))
                            {:char (char ch)})))))

(declare wrap-with-extras)

(defn parse-string
  "Parse a JSON or JSONC string into a node tree.

   Returns a root node.  For a document with leading/trailing whitespace
   or comments around a single value, returns the value node wrapped so
   that the extras are preserved.

   The returned node satisfies `(= input (node/string root))` for any
   valid JSON/JSONC input."
  [^String s]
  (when (empty? s)
    (throw (ex-info "Empty input" {})))
  (let [reader (make-reader s)
        leading (collect-extras reader)
        root (parse-value reader)
        trailing (collect-extras reader)
        remaining (peek-char reader)]
    (when (>= remaining 0)
      (throw (ex-info (str "Unexpected content after JSON value: " (char remaining))
                      {:char (char remaining)})))
    (if (and (empty? leading) (empty? trailing))
      root
      (wrap-with-extras root leading trailing))))

(defn ^:private wrap-with-extras [inner-root leading trailing]
  (let [leading-str (apply str (map node/string leading))
        trailing-str (apply str (map node/string trailing))]
    (reify
      rewrite-json.node/Node
      (tag [_] (node/tag inner-root))
      (string [_] (str leading-str (node/string inner-root) trailing-str))
      (value [_] (node/value inner-root))
      rewrite-json.node/InnerNode
      (children [_] (node/children inner-root))
      (replace-children [_ new-children]
        (wrap-with-extras (node/replace-children inner-root new-children)
                          leading trailing)))))
