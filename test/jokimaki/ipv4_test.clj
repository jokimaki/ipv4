(ns jokimaki.ipv4-test
  (:require [clojure.test :refer :all]
            [jokimaki.ipv4 :refer :all]))

(deftest test-parse-address
  (testing "simple valid addresses"
    (is (= 0 (parse-address "0.0.0.0")))
    (is (= 1 (parse-address "0.0.0.1")))
    (is (= 2 (parse-address "0.0.0.2")))
    (is (= 10 (parse-address "0.0.0.010")))
    (is (= 255 (parse-address "0.0.0.255")))
    (is (= 0xff01ff (parse-address "0.255.1.255")))
    (is (= 0xfffffffe (parse-address "255.255.255.254")))
    (is (= 0xffffffff (parse-address "255.255.255.255"))))

  (testing "invalid addresses"
    (is (thrown? AssertionError (parse-address nil)))
    (is (thrown? IllegalArgumentException (parse-address "")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.0.")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.0.0")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.-1")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.+1")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.a")))
    (is (thrown? IllegalArgumentException (parse-address "0.0.0.256")))))
