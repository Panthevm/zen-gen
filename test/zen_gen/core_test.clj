(ns zen-gen.core-test
  (:require 
    [zen-gen.core]
    [zen.validation]
    [zen.core]
    [clojure.test :refer [deftest testing]]
    [clojure.pprint]))

(defn pretty-print
  [schema data]
  (clojure.pprint/pprint schema)
  (clojure.pprint/pprint data)
  (println)
  data)

(defmacro with-context-generate
  [context schema]
  `(->> (zen-gen.core/generate          ~context '~schema)
        (pretty-print '~schema)
        (zen.validation/validate-schema ~context '~schema)
        (= (zen.validation/new-validation-acc))
        (clojure.test/is)))

(deftest generate-test

  (def context
    (zen.core/new-context))

  (testing "boolean"
    (testing "base"
      (with-context-generate context
        {:type zen/boolean}))) 

  (testing "integer"
    (testing "base"
      (with-context-generate context
        {:type zen/integer}))
    (testing "min"
      (with-context-generate context
        {:type zen/integer :min 2147483647}))
    (testing "max"
      (with-context-generate context
        {:type zen/integer :max 2147483646})
      (with-context-generate context
        {:type zen/integer :max -2147483647}))
    (testing "min-max"
      (with-context-generate context
        {:type zen/integer :min 0 :max 1})))

  (testing "number"
    (testing "base"
      (with-context-generate context
        {:type zen/number}))
    (testing "min"
      (with-context-generate context
        {:type zen/number :min 2147483647})
      (with-context-generate context
        {:type zen/number :min 2147483646.23}))
    (testing "max"
      (with-context-generate context
        {:type zen/integer :max 2147483646})
      (with-context-generate context
        {:type zen/integer :max -2147483647}))
    (testing "min-max"
      (with-context-generate context
        {:type zen/integer :min 0 :max 1})))

  (testing "string"
    (testing "base"
      (with-context-generate context
        {:type zen/string}))
    (testing "minLength"
      (with-context-generate context
        {:type zen/string :minLength 1}))
    (testing "maxLength"
      (with-context-generate context
        {:type zen/string :maxLength 1}))
    (testing "regex"
      (with-context-generate context
        {:type zen/string :regex ".*"})
      (with-context-generate context
        {:type zen/string :maxLength 3 :regex ".*"})))

  (testing "keyword"
    (testing "base"
      (with-context-generate context
        {:type zen/keyword})))

  (testing "keyword"
    (testing "base"
      (with-context-generate context
        {:type zen/symbol})))

  (testing "datetime"
    (testing "base"
      (with-context-generate context
        {:type zen/datetime})))

  (testing "set"
    (testing "base"
      (with-context-generate context
        {:type zen/set :every {:type zen/integer}}))
    (testing "minItems"
      (with-context-generate context
        {:type zen/set :minItems 15 :maxItems 20}))
    (testing "maxItems"
      (with-context-generate context
        {:type zen/set :maxItems 20}))
    (testing "subset-of"
      (with-context-generate context
        {:type zen/set :subset-of #{1 2 3 4 5} }))
    (testing "superset-of"
      (with-context-generate context
        {:type zen/set :superset-of #{1 2 3}}))

  (testing "vector"
    (testing "base"
      (with-context-generate context
        {:type zen/vector :every {:type zen/integer}}))
    (testing "minItems"
      (with-context-generate context
        {:type zen/vector :every {:type zen/integer} :minItems 1}))
    (testing "maxItems"
      (with-context-generate context
        {:type zen/vector :every {:type zen/integer} :maxItems 2})))))
