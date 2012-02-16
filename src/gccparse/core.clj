(ns gccparse.core
  (:require [clojure.string :as ss])
  (:gen-class))

(defn parse-int
  ([x           ] (if x (Integer/parseInt x)         0))
  ([x default-v ] (if x (Integer/parseInt x) default-v)))

(defn make-parser [form-map]
  (defn parse-form [[re & funcs] l]
    (if-let [[_ & mm] (re-matches re l)]
      (map  (fn [f v] (f v)) funcs mm)))
  (fn [label txt] (parse-form (form-map label) txt)))

(def gcc-parse
  (let [gcc-msg-types
        {"note"        :note  "warning"     :warning
         "error"       :error "fatal error" :fatal-error}
        s identity, int parse-int]
    (make-parser
     {:msg    [#"^([^:]+):(\d+):(?:(\d+):|) (note|warning|error|fatal error): (.*?)(?: \[([^\[\]]+)\]|)$"
                s int int gcc-msg-types s s]
      :inst   [#"^([^:]+):(\d+):(?:(\d+):|)   instantiated from (.*)$" s int int s]
      :in     [#"^([^:]+): (In .*|At .*):$" s s]
      :infile [#"^(?:In file included | +)from ([^:]+):(\d+)(?::\d+|)[,:]$" s int]
      })))

(defn parse[build_log]
  (keep-indexed
   (fn [i l]
     (or (if-let [m (gcc-parse :msg    l)] (list :msg    i m))
         (if-let [m (gcc-parse :inst   l)] (list :inst   i m))
         (if-let [m (gcc-parse :in     l)] (list :in     i m))
         (if-let [m (gcc-parse :infile l)] (list :infile i m))))
   build_log))


(defn pretty-print[mm]
  (let [fmt {:infile (fn [fname r  ] (format "%s:%d" fname r ))
             :in     (fn [fname msg] (format "%s:%s" fname msg))
             :inst   (fn [fname r c from] (format "%s:%d:%d (%s)" fname r c from))
             :msg    (fn [fname r c t msg st]
                       (format "%s:%d:%d%s (%s)" fname r c t msg))}]
    (doseq [[t l rec] mm]
      (println (format "%03d%7s {%s}" l t (apply (fmt t) rec))))))

(defn -main [& args]
  (if-let [[fname] args]
    (try (let [bl (ss/split-lines (slurp fname))
               mm (parse bl)]
           (pretty-print mm))
         (catch java.io.FileNotFoundException e
           (println "No such file:" fname)))))