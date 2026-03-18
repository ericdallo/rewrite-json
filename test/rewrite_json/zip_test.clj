(ns rewrite-json.zip-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]
   [rewrite-json.node :as node]
   [rewrite-json.zip :as z]))

(deftest down-test
  (testing "into object — first meaningful child is an entry"
    (let [loc (z/of-string "{\"a\": 1, \"b\": 2}")]
      (is (= :entry (node/tag (z/node (z/down loc)))))))

  (testing "into array — first meaningful child is a value"
    (let [loc (z/of-string "[1, 2, 3]")]
      (is (= 1 (z/value (z/down loc)))))))

(deftest right-left-test
  (testing "navigate between array elements"
    (let [loc (z/of-string "[10, 20, 30]")
          first-el (z/down loc)]
      (is (= 10 (z/value first-el)))
      (is (= 20 (z/value (z/right first-el))))
      (is (= 30 (z/value (z/right (z/right first-el)))))
      (is (nil? (z/right (z/right (z/right first-el)))))))

  (testing "navigate between object entries"
    (let [loc (z/of-string "{\"a\": 1, \"b\": 2}")
          first-entry (z/down loc)]
      (is (= :entry (node/tag (z/node first-entry))))
      (is (= :entry (node/tag (z/node (z/right first-entry)))))))

  (testing "left reverses right"
    (let [loc (z/of-string "[1, 2, 3]")
          first-el (z/down loc)
          second-el (z/right first-el)]
      (is (= 1 (z/value (z/left second-el)))))))

(deftest up-test
  (let [loc (z/of-string "[1, 2]")
        child (z/down loc)]
    (is (= :array (node/tag (z/node (z/up child)))))))

(deftest rightmost-leftmost-test
  (let [loc (z/of-string "[1, 2, 3]")
        first-el (z/down loc)]
    (is (= 3 (z/value (z/rightmost first-el))))
    (is (= 1 (z/value (z/leftmost (z/rightmost first-el)))))))

(deftest find-key-test
  (testing "finds existing key"
    (let [loc (z/of-string "{\"name\": \"Eric\", \"age\": 30}")]
      (is (some? (z/find-key loc "name")))
      (is (some? (z/find-key loc "age")))))

  (testing "returns nil for missing key"
    (let [loc (z/of-string "{\"name\": \"Eric\"}")]
      (is (nil? (z/find-key loc "missing"))))))

(deftest get-key-test
  (testing "navigates to value of key"
    (let [loc (z/of-string "{\"name\": \"Eric\", \"age\": 30}")]
      (is (= "Eric" (z/value (z/get-key loc "name"))))
      (is (= 30 (z/value (z/get-key loc "age")))))))

(deftest entry-value-test
  (testing "gets value node from entry"
    (let [loc (z/of-string "{\"count\": 42}")
          entry-loc (z/find-key loc "count")
          val-loc (z/entry-value entry-loc)]
      (is (= 42 (z/value val-loc))))))

(deftest replace-test
  (testing "replace value in object preserves formatting"
    (let [result (-> (z/of-string "{\"name\": \"old\"}")
                     (z/get-key "name")
                     (z/replace (node/string-node "new"))
                     z/root-string)]
      (is (= "{\"name\": \"new\"}" result))))

  (testing "replace preserves surrounding whitespace"
    (let [input "{\n  \"name\": \"old\"\n}"
          result (-> (z/of-string input)
                     (z/get-key "name")
                     (z/replace (node/string-node "new"))
                     z/root-string)]
      (is (= "{\n  \"name\": \"new\"\n}" result))))

  (testing "replace array element"
    (let [result (-> (z/of-string "[1, 2, 3]")
                     z/down
                     z/right ;; -> 2
                     (z/replace (node/number-node 99))
                     z/root-string)]
      (is (= "[1, 99, 3]" result)))))

(deftest remove-test
  (testing "remove first element from array"
    (let [result (-> (z/of-string "[1, 2, 3]")
                     z/down
                     z/remove
                     z/root-string)]
      (is (= "[2, 3]" result))))

  (testing "remove middle element from array"
    (let [result (-> (z/of-string "[1, 2, 3]")
                     z/down z/right
                     z/remove
                     z/root-string)]
      (is (= "[1, 3]" result))))

  (testing "remove last element from array"
    (let [result (-> (z/of-string "[1, 2, 3]")
                     z/down z/right z/right
                     z/remove
                     z/root-string)]
      (is (= "[1, 2]" result))))

  (testing "remove entry from object"
    (let [result (-> (z/of-string "{\"a\": 1, \"b\": 2}")
                     (z/find-key "a")
                     z/remove
                     z/root-string)]
      (is (not (.contains ^String result "\"a\"")))
      (is (.contains ^String result "\"b\": 2"))))

  (testing "remove last entry from object"
    (let [result (-> (z/of-string "{\"a\": 1, \"b\": 2}")
                     (z/find-key "b")
                     z/remove
                     z/root-string)]
      (is (.contains ^String result "\"a\": 1"))
      (is (not (.contains ^String result "\"b\""))))))

(deftest edit-test
  (testing "edit node with function"
    (let [result (-> (z/of-string "{\"count\": 42}")
                     (z/get-key "count")
                     (z/edit (fn [n]
                               (node/number-node (inc (node/value n)))))
                     z/root-string)]
      (is (= "{\"count\": 43}" result)))))

(deftest append-entry-test
  (testing "append to compact object"
    (let [result (-> (z/of-string "{\"a\": 1}")
                     (z/append-entry "b" (node/number-node 2))
                     z/root-string)]
      (is (.contains ^String result "\"a\": 1"))
      (is (.contains ^String result "\"b\""))
      (is (not (.contains ^String result ",,")))))

  (testing "append to multiline object"
    (let [result (-> (z/of-string "{\n  \"a\": 1,\n  \"b\": 2\n}")
                     (z/append-entry "c" (node/number-node 3))
                     z/root-string)]
      (is (.contains ^String result "\"c\": 3"))
      (is (not (.contains ^String result ",,")))
      (is (not (.contains ^String result ",\n,")))))

  (testing "append to object with trailing comma"
    (let [result (-> (z/of-string "{\"a\": 1,}")
                     (z/append-entry "b" (node/number-node 2))
                     z/root-string)]
      (is (.contains ^String result "\"b\""))
      (is (not (.contains ^String result ",,")))))

  (testing "append to empty object"
    (let [result (-> (z/of-string "{}")
                     (z/append-entry "a" (node/number-node 1))
                     z/root-string)]
      (is (.contains ^String result "\"a\"")))))

(deftest append-element-test
  (testing "append to compact array"
    (let [result (-> (z/of-string "[1, 2]")
                     (z/append-element (node/number-node 3))
                     z/root-string)]
      (is (.contains ^String result "3"))
      (is (not (.contains ^String result ",,")))))

  (testing "append to array with trailing comma"
    (let [result (-> (z/of-string "[1, 2,]")
                     (z/append-element (node/number-node 3))
                     z/root-string)]
      (is (.contains ^String result "3"))
      (is (not (.contains ^String result ",,")))))

  (testing "append to empty array"
    (let [result (-> (z/of-string "[]")
                     (z/append-element (node/number-node 1))
                     z/root-string)]
      (is (.contains ^String result "1")))))

(deftest edit-preserves-comments-test
  (testing "editing a value preserves surrounding comments"
    (let [input "{\n  // important setting\n  \"timeout\": 30\n}"
          result (-> (z/of-string input)
                     (z/get-key "timeout")
                     (z/replace (node/number-node 60))
                     z/root-string)]
      (is (.contains ^String result "// important setting"))
      (is (.contains ^String result "60"))
      (is (not (.contains ^String result "30"))))))

(deftest edit-preserves-block-comments-test
  (let [input "{\n  /* config */\n  \"port\": 8080\n}"
        result (-> (z/of-string input)
                   (z/get-key "port")
                   (z/replace (node/number-node 3000))
                   z/root-string)]
    (is (.contains ^String result "/* config */"))
    (is (.contains ^String result "3000"))))

(deftest root-string-identity-test
  (testing "of-string -> root-string is identity for unmodified tree"
    (doseq [input ["{}"
                   "[]"
                   "{\"a\": 1}"
                   "[1, 2, 3]"
                   "{\n  \"x\": true\n}"
                   "{\"nested\": {\"deep\": [1, 2]}}"]]
      (is (= input (z/root-string (z/of-string input)))
          (str "Failed roundtrip for: " input)))))
