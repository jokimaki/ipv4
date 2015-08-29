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
