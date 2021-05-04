(ns toolshed.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [toolshed.core :refer :all]))

(deftest map-values-test
  (is (= (map-vals inc {:a 1 :b 2}) {:a 2 :b 3})))

(deftest map-keys-test
  (is (= (map-keys inc {1 :a 2 :b}) {2 :a 3 :b})))