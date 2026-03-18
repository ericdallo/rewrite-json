(ns rewrite-json.node-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]
   [rewrite-json.node :as node]))

(deftest string-node-test
  (testing "constructs with correct raw and value"
    (let [n (node/string-node "hello")]
      (is (= :string (node/tag n)))
      (is (= "\"hello\"" (node/string n)))
      (is (= "hello" (node/value n)))))

  (testing "escapes double quotes"
    (let [n (node/string-node "say \"hi\"")]
      (is (= "\"say \\\"hi\\\"\"" (node/string n)))
      (is (= "say \"hi\"" (node/value n))))))

(deftest number-node-test
  (testing "integer"
    (let [n (node/number-node 42)]
      (is (= :number (node/tag n)))
      (is (= "42" (node/string n)))
      (is (= 42 (node/value n)))))

  (testing "float"
    (let [n (node/number-node 3.14)]
      (is (= "3.14" (node/string n)))
      (is (= 3.14 (node/value n))))))

(deftest boolean-node-test
  (testing "true"
    (let [n (node/boolean-node true)]
      (is (= :boolean (node/tag n)))
      (is (= "true" (node/string n)))
      (is (= true (node/value n)))))

  (testing "false"
    (let [n (node/boolean-node false)]
      (is (= "false" (node/string n)))
      (is (= false (node/value n))))))

(deftest null-node-test
  (let [n (node/null-node)]
    (is (= :null (node/tag n)))
    (is (= "null" (node/string n)))
    (is (nil? (node/value n)))))

(deftest whitespace-node-test
  (let [n (node/whitespace-node "  \n  ")]
    (is (= :whitespace (node/tag n)))
    (is (= "  \n  " (node/string n)))
    (is (nil? (node/value n)))
    (is (node/whitespace? n))))

(deftest comment-node-test
  (testing "line comment"
    (let [n (node/comment-node "// hello" :line)]
      (is (= :comment (node/tag n)))
      (is (= "// hello" (node/string n)))
      (is (node/comment? n))))

  (testing "block comment"
    (let [n (node/comment-node "/* block */" :block)]
      (is (= "/* block */" (node/string n))))))

(deftest comma-node-test
  (let [n (node/comma-node)]
    (is (= :comma (node/tag n)))
    (is (= "," (node/string n)))
    (is (node/comma? n))))

(deftest colon-node-test
  (let [n (node/colon-node)]
    (is (= :colon (node/tag n)))
    (is (= ":" (node/string n)))
    (is (node/colon? n))))

(deftest entry-node-test
  (testing "two-arg constructor"
    (let [n (node/entry-node (node/string-node "key")
                             (node/number-node 42))]
      (is (= :entry (node/tag n)))
      (is (= "\"key\": 42" (node/string n)))
      (is (match? ["key" 42] (node/value n)))))

  (testing "entry? predicate"
    (is (node/entry? (node/entry-node (node/string-node "k")
                                      (node/string-node "v"))))
    (is (not (node/entry? (node/string-node "x"))))))

(deftest array-node-test
  (let [n (node/array-node [(node/number-node 1)
                            (node/->CommaNode ", ")
                            (node/number-node 2)])]
    (is (= :array (node/tag n)))
    (is (= "[1, 2]" (node/string n)))
    (is (= [1 2] (node/value n)))))

(deftest object-node-test
  (let [n (node/object-node [(node/entry-node (node/string-node "a")
                                              (node/number-node 1))
                             (node/->CommaNode ", ")
                             (node/entry-node (node/string-node "b")
                                              (node/number-node 2))])]
    (is (= :object (node/tag n)))
    (is (= "{\"a\": 1, \"b\": 2}" (node/string n)))
    (is (= {"a" 1 "b" 2} (node/value n)))))

(deftest value->node-test
  (testing "nil"
    (is (= "null" (node/string (node/value->node nil)))))

  (testing "string"
    (is (= "\"hello\"" (node/string (node/value->node "hello")))))

  (testing "number"
    (is (= "42" (node/string (node/value->node 42)))))

  (testing "boolean"
    (is (= "true" (node/string (node/value->node true)))))

  (testing "vector"
    (is (= "[1, 2, 3]" (node/string (node/value->node [1 2 3]))))))

(deftest predicates-test
  (testing "value-node?"
    (is (node/value-node? (node/string-node "x")))
    (is (node/value-node? (node/number-node 1)))
    (is (node/value-node? (node/boolean-node true)))
    (is (node/value-node? (node/null-node)))
    (is (node/value-node? (node/array-node [])))
    (is (node/value-node? (node/object-node [])))
    (is (not (node/value-node? (node/whitespace-node " ")))))

  (testing "container?"
    (is (node/container? (node/object-node [])))
    (is (node/container? (node/array-node [])))
    (is (not (node/container? (node/string-node "x")))))

  (testing "printable?"
    (is (node/printable? (node/string-node "x")))
    (is (not (node/printable? (node/whitespace-node " "))))
    (is (not (node/printable? (node/comment-node "// c" :line))))))

(deftest key-node-test
  (let [entry (node/entry-node (node/string-node "mykey")
                               (node/number-node 99))]
    (is (= "mykey" (node/value (node/key-node entry))))))

(deftest value-of-entry-test
  (let [entry (node/entry-node (node/string-node "k")
                               (node/string-node "v"))]
    (is (= "v" (node/value (node/value-of-entry entry))))))
