(ns jokimaki.ipv4
  (:require [clojure.string :as s]))

(defn ^Long parse-address [^String s]
  {:pre [(string? s)]}
  (let [splitted (rest (re-matches #"(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})" s))
        digits (map #(Long/parseLong % 10) splitted)]

    (when-not (and (= 4 (count splitted)) (every? #(<= 0 % 255) digits))
      (throw (IllegalArgumentException. (str "Not a valid IP: " s))))

    (loop [ip (first digits), digits (rest digits)]
      (if (seq digits)
        (recur (bit-or (bit-shift-left ip 8) (first digits)), (rest digits))
        ip))))

(defn ^String format-address [^Long ip]
  {:pre [(number? ip)]}
  (when-not (<= 0 ip 0xffffffff) (throw (IllegalArgumentException. (str "Out of IPv4 range"))))

  (apply format "%d.%d.%d.%d"
    (loop [result '(), ip-bits ip]
      (if (>= (count result) 4)
        result
        (recur (conj result (bit-and 0xff ip-bits)) (bit-shift-right ip-bits 8))))))

(defn- mask-bytes [^Long mask-size]
  (when-not (<= 0 mask-size 32)
    (throw (IllegalArgumentException. (str "Mask must be 0-32: " mask-size))))

  (let [ones 0xffffffff
        inverse-mask (bit-shift-right ones mask-size)
        mask (bit-xor ones inverse-mask)]
    [inverse-mask mask]))

(defn network-min-max
  "Returns the first and last IP addresses in a given network as a pair of Longs.
   Use `format-address` to get a human readable IP address."
  [^String network]
  {:pre [(string? network)]}
  (when-not (re-matches #"^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/\d{1,2}$" network)
    (throw (IllegalArgumentException. (str "Not a valid IP/mask: " network))))

  (let [[mask-net mask-size] (s/split network #"/")
        net (parse-address mask-net)
        mask (mask-bytes (Long/parseLong mask-size))
        min (bit-and net (last mask))
        max (bit-or net (first mask))]
    [min max]))

(defn address-in-network? [^String address ^String network]
  (let [ip (parse-address address)
        [min max] (network-min-max network)]
    (<= min ip max)))
