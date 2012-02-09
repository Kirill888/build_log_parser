(ns gccparse.core
  (:require [clojure.string :as ss])
  (:gen-class))


(defn match_infile[l]
  (if-let [m (re-matches #"^In file included from [^:]+:\d+(?::\d+)[,:]$" l)]
    (list l)))

(defn match_hdr[l]
  (if-let [[_ fname msg] (re-matches #"^([^:]+): (In .* ‘.*’|At .*):$" l)]
    (list fname, msg)))

(defn match_we[l]
  (defn parse-msg [msg]
    (if-let [[_ body flags] (re-matches #"^(.*) \[([^\[\]]+)\]$" msg)]
      (list body flags)
      (list msg "")))

  (if-let [[_,fname,r,c,t,msg]
           (re-matches #"^([^:]+):(\d+):(?:(\d+):|) (warning|error): (.*)$" l)]
    (list* fname
           (Integer/parseInt r)
           (if c (Integer/parseInt c) 0)
           ({"warning" :warning "error" :error} t)
           (parse-msg msg))))

(defn parse[build_log]
  (defn match[i l]
    (or (if-let [m (match_we     l)] (list :msg    i m))
        (if-let [m (match_hdr    l)] (list :in     i m))
        (if-let [m (match_infile l)] (list :infile i m))))
  (keep-indexed match build_log))


(defn pretty-print[mm]
  (let [fmt {:infile (fn [l] (format "%s" l ))
             :in     (fn [fname msg] (format "%s:%s" fname msg))
             :msg    (fn [fname r c t msg st]
                       (format "%s:%d:%d (%s)" fname r c msg))}]
    (doseq [[t l rec] mm]
      (println (format "%03d%7s {%s}" l t (apply (fmt t) rec))))))

(defn -main [& args]
  (println "Welcome to my project! These are your args:" args))