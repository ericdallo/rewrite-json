(ns rewrite-json.parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]
   [rewrite-json.node :as node]
   [rewrite-json.parser :as parser]))

(defn- roundtrip [s]
  (node/string (parser/parse-string s)))

(deftest parse-string-primitive-test
  (testing "simple string"
    (let [n (parser/parse-string "\"hello\"")]
      (is (= :string (node/tag n)))
      (is (= "hello" (node/value n)))
      (is (= "\"hello\"" (roundtrip "\"hello\"")))))

  (testing "string with escapes"
    (is (= "\"hello\\nworld\"" (roundtrip "\"hello\\nworld\"")))
    (is (= "\"say \\\"hi\\\"\"" (roundtrip "\"say \\\"hi\\\"\""))))

  (testing "backslash-n vs newline"
    (is (= "hello\nworld" (node/value (parser/parse-string "\"hello\\nworld\""))))
    (is (= "a\\nb" (node/value (parser/parse-string "\"a\\\\nb\"")))))

  (testing "double backslash"
    (let [json-text "\"\\\\\\\\\""]  ;; JSON: "\\\\" (two backslashes)
      (is (= json-text (roundtrip json-text)))
      (is (= "\\\\" (node/value (parser/parse-string json-text))))))

  (testing "unicode escapes"
    (is (= "\"\\u0041\"" (roundtrip "\"\\u0041\"")))
    (is (= "A" (node/value (parser/parse-string "\"\\u0041\"")))))

  (testing "all escape types roundtrip"
    (doseq [esc ["\\/" "\\b" "\\f" "\\r" "\\t"]]
      (let [input (str "\"" esc "\"")]
        (is (= input (roundtrip input))))))

  (testing "value->string-node roundtrip"
    (let [val "line1\nline2\ttab\\backslash\"quote"]
      (is (= val (-> val node/string-node node/string parser/parse-string node/value))))))

(deftest parse-number-test
  (testing "integer"
    (let [n (parser/parse-string "42")]
      (is (= :number (node/tag n)))
      (is (= 42 (node/value n)))
      (is (= "42" (roundtrip "42")))))

  (testing "negative integer"
    (is (= "-7" (roundtrip "-7")))
    (is (= -7 (node/value (parser/parse-string "-7")))))

  (testing "float"
    (is (= "3.14" (roundtrip "3.14")))
    (is (= 3.14 (node/value (parser/parse-string "3.14")))))

  (testing "scientific notation"
    (is (= "1e10" (roundtrip "1e10")))
    (is (= "1.5E-3" (roundtrip "1.5E-3")))))

(deftest parse-boolean-test
  (testing "true"
    (let [n (parser/parse-string "true")]
      (is (= :boolean (node/tag n)))
      (is (= true (node/value n)))
      (is (= "true" (roundtrip "true")))))

  (testing "false"
    (is (= "false" (roundtrip "false")))
    (is (= false (node/value (parser/parse-string "false"))))))

(deftest parse-null-test
  (let [n (parser/parse-string "null")]
    (is (= :null (node/tag n)))
    (is (nil? (node/value n)))
    (is (= "null" (roundtrip "null")))))

(deftest parse-empty-array-test
  (is (= "[]" (roundtrip "[]")))
  (is (= "[ ]" (roundtrip "[ ]")))
  (is (= "[  ]" (roundtrip "[  ]"))))

(deftest parse-simple-array-test
  (is (= "[1, 2, 3]" (roundtrip "[1, 2, 3]")))
  (is (= [1 2 3] (node/value (parser/parse-string "[1, 2, 3]")))))

(deftest parse-array-preserves-formatting-test
  (testing "extra spaces"
    (is (= "[  1 ,  2 ,  3  ]" (roundtrip "[  1 ,  2 ,  3  ]"))))

  (testing "multiline"
    (let [input "[\n  1,\n  2,\n  3\n]"]
      (is (= input (roundtrip input)))))

  (testing "nested"
    (let [input "[[1, 2], [3, 4]]"]
      (is (= input (roundtrip input)))
      (is (= [[1 2] [3 4]] (node/value (parser/parse-string input)))))))

(deftest parse-empty-object-test
  (is (= "{}" (roundtrip "{}")))
  (is (= "{ }" (roundtrip "{ }")))
  (is (= "{  }" (roundtrip "{  }"))))

(deftest parse-simple-object-test
  (let [input "{\"name\": \"Eric\", \"age\": 30}"]
    (is (= input (roundtrip input)))
    (is (= {"name" "Eric" "age" 30}
           (node/value (parser/parse-string input))))))

(deftest parse-object-preserves-formatting-test
  (testing "extra spaces around colon"
    (is (= "{\"a\" : \"b\"}" (roundtrip "{\"a\" : \"b\"}"))))

  (testing "no spaces"
    (is (= "{\"a\":\"b\"}" (roundtrip "{\"a\":\"b\"}"))))

  (testing "multiline with indentation"
    (let [input "{\n  \"name\": \"Eric\",\n  \"age\": 30\n}"]
      (is (= input (roundtrip input)))))

  (testing "4-space indentation"
    (let [input "{\n    \"a\": 1,\n    \"b\": 2\n}"]
      (is (= input (roundtrip input)))))

  (testing "tab indentation"
    (let [input "{\n\t\"a\": 1,\n\t\"b\": 2\n}"]
      (is (= input (roundtrip input))))))

(deftest parse-nested-object-test
  (let [input "{\"outer\": {\"inner\": 42}}"]
    (is (= input (roundtrip input)))
    (is (= {"outer" {"inner" 42}}
           (node/value (parser/parse-string input))))))

(deftest parse-line-comment-test
  (testing "comment before value in object"
    (let [input "{\n  // This is a name\n  \"name\": \"Eric\"\n}"]
      (is (= input (roundtrip input)))))

  (testing "comment after entry"
    (let [input "{\"a\": 1, // inline comment\n\"b\": 2}"]
      (is (= input (roundtrip input))))))

(deftest parse-block-comment-test
  (testing "block comment in object"
    (let [input "{\n  /* name field */\n  \"name\": \"Eric\"\n}"]
      (is (= input (roundtrip input)))))

  (testing "inline block comment"
    (let [input "{\"a\": /* value */ 1}"]
      (is (= input (roundtrip input))))))

(deftest parse-trailing-comma-test
  (testing "trailing comma in object"
    (let [input "{\"a\": 1, \"b\": 2,}"]
      (is (= input (roundtrip input)))
      (is (= {"a" 1 "b" 2}
             (node/value (parser/parse-string input))))))

  (testing "trailing comma in array"
    (let [input "[1, 2, 3,]"]
      (is (= input (roundtrip input)))
      (is (= [1 2 3] (node/value (parser/parse-string input)))))))

(deftest parse-leading-trailing-whitespace-test
  (testing "leading whitespace"
    (is (= "  {}" (roundtrip "  {}"))))

  (testing "trailing whitespace"
    (is (= "{}\n" (roundtrip "{}\n"))))

  (testing "both"
    (is (= "\n{}\n" (roundtrip "\n{}\n")))))

(deftest parse-vscode-settings-test
  (let [input "{\n  // Editor settings\n  \"editor.fontSize\": 14,\n  \"editor.tabSize\": 2,\n\n  // Terminal\n  \"terminal.integrated.shell.linux\": \"/bin/bash\"\n}"]
    (is (= input (roundtrip input)))))

(deftest parse-tsconfig-test
  (let [input "{\n  \"compilerOptions\": {\n    \"target\": \"es2020\",\n    \"module\": \"commonjs\",\n    \"strict\": true\n  },\n  \"include\": [\"src/**/*\"],\n  \"exclude\": [\"node_modules\"]\n}"]
    (is (= input (roundtrip input)))))

(deftest parse-empty-input-test
  (is (thrown? clojure.lang.ExceptionInfo (parser/parse-string "")))
  (is (thrown? clojure.lang.ExceptionInfo (parser/parse-string "   "))))

(deftest parse-trailing-garbage-test
  (is (thrown? clojure.lang.ExceptionInfo (parser/parse-string "42 garbage")))
  (is (thrown? clojure.lang.ExceptionInfo (parser/parse-string "{} []"))))

(deftest parse-large-number-test
  (testing "large integer doesn't crash"
    (let [n (parser/parse-string "99999999999999999999999999999")]
      (is (some? (node/value n)))
      (is (= "99999999999999999999999999999" (roundtrip "99999999999999999999999999999"))))))

(deftest parse-missing-colon-test
  (is (thrown? clojure.lang.ExceptionInfo (parser/parse-string "{\"key\" \"value\"}"))))
