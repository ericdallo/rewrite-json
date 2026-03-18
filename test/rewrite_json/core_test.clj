(ns rewrite-json.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]
   [rewrite-json.core :as rj]))

(deftest roundtrip-test
  (testing "simple object roundtrips"
    (let [input "{\"name\": \"Eric\", \"age\": 30}"]
      (is (= input (rj/to-string (rj/parse-string input))))))

  (testing "multiline with comments roundtrips"
    (let [input "{\n  // name\n  \"name\": \"Eric\",\n  \"age\": 30\n}"]
      (is (= input (rj/to-string (rj/parse-string input)))))))

(deftest get-in-test
  (testing "top-level key"
    (let [root (rj/parse-string "{\"name\": \"Eric\"}")]
      (is (= "Eric" (rj/get-in root ["name"])))))

  (testing "nested key"
    (let [root (rj/parse-string "{\"a\": {\"b\": {\"c\": 42}}}")]
      (is (= 42 (rj/get-in root ["a" "b" "c"])))))

  (testing "array index"
    (let [root (rj/parse-string "{\"items\": [10, 20, 30]}")]
      (is (= 20 (rj/get-in root ["items" 1])))))

  (testing "missing path returns not-found"
    (let [root (rj/parse-string "{\"a\": 1}")]
      (is (nil? (rj/get-in root ["missing"])))
      (is (= :default (rj/get-in root ["missing"] :default))))))

(deftest assoc-in-replace-test
  (testing "replace existing value preserves formatting"
    (let [input "{\n  \"name\": \"old\",\n  \"age\": 25\n}"
          root (rj/parse-string input)
          result (rj/to-string (rj/assoc-in root ["name"] "new"))]
      (is (= "{\n  \"name\": \"new\",\n  \"age\": 25\n}" result))))

  (testing "replace nested value"
    (let [input "{\"config\": {\"port\": 8080}}"
          root (rj/parse-string input)
          result (rj/to-string (rj/assoc-in root ["config" "port"] 3000))]
      (is (= "{\"config\": {\"port\": 3000}}" result))))

  (testing "replace with different type"
    (let [root (rj/parse-string "{\"enabled\": true}")]
      (is (= "{\"enabled\": false}"
             (rj/to-string (rj/assoc-in root ["enabled"] false))))))

  (testing "replace array element"
    (let [root (rj/parse-string "[1, 2, 3]")]
      (is (= "[1, 99, 3]"
             (rj/to-string (rj/assoc-in root [1] 99)))))))

(deftest assoc-in-append-test
  (testing "assoc-in new key appends entry"
    (let [root (rj/parse-string "{\"a\": 1}")
          result (rj/to-string (rj/assoc-in root ["b"] 2))]
      (is (.contains ^String result "\"b\""))
      (is (.contains ^String result "2"))
      ;; Original preserved
      (is (.contains ^String result "\"a\": 1")))))

(deftest update-in-test
  (testing "increment a number"
    (let [root (rj/parse-string "{\"count\": 10}")
          result (rj/to-string (rj/update-in root ["count"] inc))]
      (is (= "{\"count\": 11}" result))))

  (testing "update nested value"
    (let [root (rj/parse-string "{\"config\": {\"retries\": 3}}")
          result (rj/to-string (rj/update-in root ["config" "retries"] + 2))]
      (is (= "{\"config\": {\"retries\": 5}}" result)))))

(deftest dissoc-in-test
  (testing "remove first key from object"
    (let [root (rj/parse-string "{\"a\": 1, \"b\": 2}")
          result (rj/to-string (rj/dissoc-in root ["a"]))]
      (is (not (.contains ^String result "\"a\"")))
      (is (.contains ^String result "\"b\": 2"))))

  (testing "remove last key from object"
    (let [root (rj/parse-string "{\"a\": 1, \"b\": 2}")
          result (rj/to-string (rj/dissoc-in root ["b"]))]
      (is (.contains ^String result "\"a\": 1"))
      (is (not (.contains ^String result "\"b\"")))))

  (testing "remove from single-entry object"
    (let [root (rj/parse-string "{\"a\": 1}")
          result (rj/to-string (rj/dissoc-in root ["a"]))]
      (is (not (.contains ^String result "\"a\"")))))

  (testing "remove nested key"
    (let [root (rj/parse-string "{\"config\": {\"a\": 1, \"b\": 2}}")
          result (rj/to-string (rj/dissoc-in root ["config" "a"]))]
      (is (not (.contains ^String result "\"a\"")))
      (is (.contains ^String result "\"b\": 2"))))

  (testing "missing key returns root unchanged"
    (let [root (rj/parse-string "{\"a\": 1}")
          result (rj/dissoc-in root ["missing"])]
      (is (= "{\"a\": 1}" (rj/to-string result))))))

(deftest edit-vscode-settings-test
  (testing "change font size in VS Code settings with comments"
    (let [input "{\n  // Editor config\n  \"editor.fontSize\": 14,\n  \"editor.tabSize\": 2\n}"
          root (rj/parse-string input)
          result (rj/to-string (rj/assoc-in root ["editor.fontSize"] 16))]
      ;; Comment preserved
      (is (.contains ^String result "// Editor config"))
      ;; Value changed
      (is (.contains ^String result "16"))
      (is (not (.contains ^String result "14")))
      ;; Other values preserved
      (is (.contains ^String result "\"editor.tabSize\": 2")))))

(deftest edit-package-json-test
  (testing "update version in package.json-like structure"
    (let [input "{\n  \"name\": \"my-app\",\n  \"version\": \"1.0.0\",\n  \"dependencies\": {\n    \"react\": \"^17.0.0\"\n  }\n}"
          root (rj/parse-string input)
          result (rj/to-string (rj/assoc-in root ["version"] "2.0.0"))]
      (is (.contains ^String result "\"version\": \"2.0.0\""))
      ;; Everything else preserved
      (is (.contains ^String result "\"name\": \"my-app\""))
      (is (.contains ^String result "\"react\": \"^17.0.0\"")))))

(deftest edit-nested-dependency-test
  (testing "update nested dependency version"
    (let [input "{\n  \"dependencies\": {\n    \"react\": \"^17.0.0\",\n    \"lodash\": \"^4.17.0\"\n  }\n}"
          root (rj/parse-string input)
          result (rj/to-string (rj/assoc-in root ["dependencies" "react"] "^18.0.0"))]
      (is (.contains ^String result "\"react\": \"^18.0.0\""))
      (is (.contains ^String result "\"lodash\": \"^4.17.0\"")))))
