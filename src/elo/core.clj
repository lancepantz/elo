(ns elo.core
  (:require [clojure.string :as str]))

(def start 1000)

(defn read-file [fn]
  (str/split-lines (slurp fn)))

(defn parse-line [l]
  (str/split l #"\s"))

(defn assoc-head [ks result-line]
  (into {} (map vector ks result-line)))

(defn get-scores [results]
  (reduce (fn [acc line]
            (into acc {(line "playerB") start (line "playerA") start}))
          {}
          results))

(defn invert [s]
  (if (= s "A") "B" "A"))

(defn new-score [current player opponent won]
  (+  (* 400 (if won 1 -1))
      (current opponent)))

(defn go []
  (let [results (map parse-line (read-file "/Users/lance/code/elo/sample.txt"))
        keys (first results)
        results (map (partial assoc-head keys) (rest results))
        scores (atom (get-scores results))]
    (doseq [game results]
      (if (nil? (game "outcome"))
        (let [a (@scores (game "playerA"))
              b (@scores (game "playerB"))]
          (swap! scores assoc (game "playerA") b (game "playerB") a))
        (let [winner-key (game (str "player" (game "outcome")))
              loser-key (game (str "player" (invert (game "outcome"))))
              winner-value (new-score @scores winner-key loser-key true)
              loser-value (new-score @scores loser-key winner-key false)]
          (swap! scores assoc winner-key winner-value loser-key loser-value))))
    (prn @scores)))