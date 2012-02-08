(ns gccparse.core
  (:require [clojure.string :as ss])
  (:gen-class))


(defn match_infile[l]
  (if-let [m (re-matches #"^In file included from [^:]+:\d+(?::\d+)[,:]$" l)]
    l))

(defn match_hdr[l]
  (if-let [m (re-matches #"^([^:]+): (In .* ‘.*’|At .*):$" l)]
    (apply (fn [_,fname, msg](list fname, msg)) m)))

(defn match_we[l]
  (defn parse-msg [msg]
    (if-let [m (re-matches #"^(.*) \[([^\[\]]+)\]$" msg)]
      (list (m 1) (m 2))
      (list msg "")))
  
  (if-let [m (re-matches #"^([^:]+):(\d+):(?:(\d+):|) (warning|error): (.*)$" l)]
    (apply (fn [_,fname,r,c,t,msg]
             (list*
              fname
              (Integer/parseInt r)
              (if c (Integer/parseInt c) 0)
              ({"warning" :warning "error" :error} t)
              (parse-msg msg)))
           m)))

(defn parse[build_log]
  (defn match[i l]
    (if-let     [m (match_we     l)] (list :msg    i m)
      (if-let   [m (match_hdr    l)] (list :in     i m)
        (if-let [m (match_infile l)] (list :infile i m)))))
  (keep-indexed match build_log))

(defn -main [& args]
  (println "Welcome to my project! These are your args:" args))