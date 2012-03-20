(ns krailoj.core
 (:require [clj-http.client :as http-client]
          [clojure.data.csv :as csv]
          [irclj.core :as irclj]
          [clojure.string :as string]))


(def csv-url "https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv")

; ref to opinions vector of opinion maps { :match :response }
(def opinions (ref []))

(defn get-new-opinions [] 
 (let [f ((http-client/get csv-url) :body)
       cf (csv/read-csv f)
       ops (map (fn [l] 
            {
             ; the (?i) appears make it case insensitive
             :match (re-pattern (str "(?i)" (first l))) 
             :response (str "? " (string/join "." (rest l)))
            }) 
            cf)]
    (dosync
     (ref-set opinions ops))))

(defn responses-for-line [l]
 (reduce 
  (fn [responses opinion] 
   (if-let 
    [grp (re-find (opinion :match) l)] 
     (conj responses (str grp (opinion :response)))
     responses
     )) 
   '() 
   @opinions
  ))

(defn handle-line [irc msg]
 (let [user (msg :nick)
       chan (first (msg :params))
       said (apply str (rest (msg :params)))
       responses (responses-for-line said)]

    (doseq [res responses] (irclj/message irc chan (str (msg :nick) ": " res)))))

(defn make-irc-connection []
 (irclj/connect "irc.madhax.net" 6667 "krailjOS" :real-name "krailjOS" :mode 0 :callbacks {:privmsg handle-line}))

(defn -main [& args]
  (make-irc-connection) 
  (get-new-opinions))

