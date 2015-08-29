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

(deftest test-format-address
  (testing "simple valid addresses"
    (is (= "0.0.0.0" (format-address 0)))
    (is (= "0.0.0.1" (format-address 1)))
    (is (= "0.0.1.0" (format-address 0x100)))
    (is (= "0.1.0.0" (format-address 0x10000)))
    (is (= "1.0.0.0" (format-address 0x1000000)))
    (is (= "0.0.0.255" (format-address 0xff)))
    (is (= "255.255.255.255" (format-address 0xffffffff)))
    (is (= "0.255.1.255" (format-address 0xff01ff))))

  (testing "invalid addresses"
    (is (thrown? AssertionError (format-address nil)))
    (is (thrown? IllegalArgumentException (format-address -1)))
    (is (thrown? IllegalArgumentException (format-address Long/MAX_VALUE)))
    (is (thrown? IllegalArgumentException (format-address Long/MIN_VALUE)))
    (is (thrown? IllegalArgumentException (format-address Integer/MIN_VALUE)))
    (is (thrown? IllegalArgumentException (format-address (+ 2 (* 2 Integer/MAX_VALUE)))))
    (is (thrown? IllegalArgumentException (format-address 0x100000000)))))

(deftest test-parse-format-combination
  (let [pf (comp format-address parse-address)
        fp (comp parse-address format-address)]

    (testing "parse->format"
      (is (= "0.0.0.0" (pf "0.0.0.0")))
      (is (= "172.16.0.1" (pf "172.16.0.1")))
      (is (= "192.168.100.8" (pf "192.168.100.8")))
      (is (= "255.255.255.255" (pf "255.255.255.255"))))

    (testing "format->parse"
      (is (= 0 (fp 0)))
      (is (= 123456 (fp 123456)))
      (is (= 12345678 (fp 12345678)))
      (is (= 0xffffffff (fp 0xffffffff))))))

(deftest test-network-min-max
  (testing "private IPv4 address spaces"
    (let [min-max-formatted (comp (partial map format-address) network-min-max)
         private-10 (min-max-formatted "10.0.0.0/8")
         private-172 (min-max-formatted "172.16.0.0/12")
         private-192 (min-max-formatted "192.168.0.0/16")]
     (is (= "10.0.0.0" (first private-10)))
     (is (= "10.255.255.255" (second private-10)))
     (is (= "172.16.0.0" (first private-172)))
     (is (= "172.31.255.255" (last private-172)))
     (is (= "192.168.0.0" (first private-192)))
     (is (= "192.168.255.255" (last private-192)))))

  (testing "invalid network syntax"
    (is (thrown? AssertionError (network-min-max nil)))
    (is (thrown? IllegalArgumentException (network-min-max "0.0.0.0/-1")))
    (is (thrown? IllegalArgumentException (network-min-max "0.0.0.0/33")))
    (is (thrown? IllegalArgumentException (network-min-max "0.0.0.0/1/2")))
    (is (thrown? IllegalArgumentException (network-min-max "10.0.0.0/255.0.0.0"))
      "(Syntax not supported yet)")))

(deftest test-address-in-network
  (is (address-in-network? "10.0.0.0" "10.0.0.0/8"))
  (is (address-in-network? "10.0.0.1" "10.0.0.0/8"))
  (is (address-in-network? "10.255.255.254" "10.0.0.0/8"))
  (is (address-in-network? "10.255.255.255" "10.0.0.0/8"))
  (is (not (address-in-network? "9.255.255.255" "10.0.0.0/8")))
  (is (not (address-in-network? "11.0.0.0" "10.0.0.0/8"))))
