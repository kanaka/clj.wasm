#!/usr/bin/env bb

(ns spec
  (:require [babashka.deps :as deps]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(require '[bartleby.core :as b]
         '[bartleby.parser :as parser]
         '[bartleby.math :as math])

(defn list-rst-files [dir]
  (->> (io/file dir)
       .listFiles
       (filter #(.isFile %))
       (filter #(= (.getName %) "types.rst"))
       (map #(.getPath %))))

(defn extract-math-sections [content]
  (let [sections (b/parse content)]
    (->> sections
         (filter #(= (:type %) :math))
         (map :content))))

(defn parse-production [math-content]
  (try 
    (let [productions (b/parse-math math-content)]
      (->> productions
           (filter #(= (:type %) :production))
           (map (fn [prod]
                 {:name (:name prod)
                  :type (:type-name prod)
                  :rules (:rules prod)}))))
    (catch Exception e
      (println "Failed to parse math content:" math-content)
      (println "Error:" (.getMessage e))
      nil)))

(defn process-file [file]
  (let [content (slurp file)
        math-sections (extract-math-sections content)]
    (when (seq math-sections)
      (println "\nFile:" file)
      (println "Found" (count math-sections) "math sections:\n")
      (doseq [section math-sections]
        (when-let [productions (parse-production section)]
          (doseq [prod productions]
            (println "Production:")
            (pprint prod)
            (println)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [dir (or (first *command-line-args*) ".")]
    (doseq [file (list-rst-files dir)]
      (process-file file))))
