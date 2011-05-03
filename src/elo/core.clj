(ns elo.core
  (:require [clojure.string :as str])
  (:use clojure.contrib.math)
  )

(def initial-rating 1500)
(def k-factor 15)

(defn read-file [fn]
  (str/split-lines (slurp fn)))

(defn parse-line [l]
  (str/split (str/upper-case l) #"\s"))

(defn assoc-head [keys result-line]
  (into {} (map vector keys result-line)))

(defn init-ratings [results]
  (reduce (fn [acc line]
            (into acc {(line "PLAYERA") initial-rating (line "PLAYERB") initial-rating}))
          {}
          results))

(defn score [outcome]
  (if (= outcome "D")
    0.5
    (if (= outcome "A") 1.0 0.0)))

(defn new-rating [ratings player opponent score]
  (let [p-rating (ratings player)
        o-rating (ratings opponent)
        expected (/ (+ 1.0 (expt 10.0 (/ (- p-rating o-rating) 400.0))))]
    (+ p-rating (* k-factor (- score expected)))))

(defn go []
  (let [results (map parse-line (read-file "/Users/egamble/clojure/elo/sample.txt"))
        keys (first results)
        results (map (partial assoc-head keys) (rest results))
        ratings (atom (init-ratings results))]
    (doseq [game results]
      (let [a (game "PLAYERA")
            b (game "PLAYERB")
            a-score (score (game "OUTCOME"))
            a-rating (new-rating @ratings a b a-score)
            b-rating (new-rating @ratings b a (- 1.0 a-score))]
        (swap! ratings assoc a a-rating b b-rating)))
    (prn @ratings)))