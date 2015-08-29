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
