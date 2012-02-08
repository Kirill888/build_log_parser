(ns gccparse.core
  (:require [clojure.string :as ss])
  (:gen-class))


(defn match_infile[l]
  (let [m (re-matches #"^In file included from [^:]+:\d+(?::\d+)[,:]$" l)]
    (if m true)))

(defn match_hdr[l]
  (let [m (re-matches #"^([^:]+): (In .* ‘.*’|At .*):$" l)]
    (if m (apply (fn [_,fname, msg]
                   (list fname, msg)) m))))

(defn match_we[l]
  (defn parse-msg [msg]
    (let [m (re-matches #"^(.*) \[([^\[\]]+)\]$" msg)]
      (if m
        (list (m 1) (m 2))
        (list msg ""))))
  
  (let [m (re-matches #"^([^:]+):(\d+):(?:(\d+):|) (warning|error): (.*)$" l)]
    (if m (apply (fn [_,fname,r,c,t,msg]
               (list* fname
                      (Integer/parseInt r)
                      (if c (Integer/parseInt c) 0)
                      ({"warning" :warning "error" :error} t)
                      (parse-msg msg)))
                 m))))


(defn parse[build_log]
  (defn match[i l]
    (let [w (match_we l)]
      (if w
        (list :msg i w)
        (let [h (match_hdr l)]
          (if h (list :in i h)
              (if (match_infile l) (list :infile i l))
          )))))
  (keep-indexed match build_log))

(defn -main [& args]
  (println "Welcome to my project! These are your args:" args))


;; #"^([^:]+):(\d+):(?:(\d+):|) (warning|error): (.*)$"
;; #"^(.*) \[([^\[\]]+)\]$"
;; #"^[^:]+: (:?In .* ‘.*’|At .*):$"

;  rx  = "(?:^[^:]+: (In .*|At .*):$" +\
;        "(?:\n^[^:]+:\d+:(?:\d+:|)\s+instantiated from .*$){0,}){0,1}" +\
;        "\n^([^:]+):(\d+):(?:(\d+):|) (warning|error): (.*)$"
