(ns krailoj.core
 (:require [clj-http.client :as http-client]
          [clojure.data.csv :as csv]
          [irclj.core :as irclj]
          [clojure.string :as string]))


(def csv-url "https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv")

; ref to opinions vector of opinion maps { :match :response }
(def opinions (ref []))

(defn 
  get-new-opinions 
  "Modify opinions by downloading and parsing a new opinions file"
  [] 
  (let [f ((http-client/get csv-url) :body)
        cf (csv/read-csv f)
        ops (map (fn [l] {
                 ; the (?i) appears make it case insensitive
                 :match (re-pattern (str "(?i)" "(\\(|\\s|^)(" (first l) ")(\\s|\\)|\\.|\\?|\\!|$)")) 
                 :response (str "? " (string/join ". " (rest l)))
                 }) cf)]
    (dosync
      (ref-set opinions ops))))

(defn 
  responses-for-line 
  "Return a list of responses for a given line, or empty list for no response"
  [l]
  (println "searching " (count @opinions))
  (reduce 
    (fn [responses opinion] 
      (if-let [grp (re-find (opinion :match) l)] 
        (conj responses (str (first grp) (opinion :response)))
        responses)) 
    '() 
    @opinions
    ))

(defn 
  handle-line 
  "Handle a line from irclj"
  [irc msg]
 (let [user (msg :nick)
       chan (first (msg :params))
       said (apply str (rest (msg :params)))
       responses (responses-for-line said)]
   (println "handling " chan said " responds with " (first responses))

   (doseq [res responses] (irclj/message irc chan (str (msg :nick) ": " res)))))

(def handle-line-ref (ref handle-line))

(defn 
  make-irc-connection 
  "Create the irclj connection/irc var"
  []
 (irclj/connect "irc.madhax.net" 6667 "krailjOS" :real-name "krailjOS" :mode 0 :callbacks {:privmsg (fn [& v] (apply @handle-line-ref v))}))

(defn 
  -main 
  "Main function"
  [& args]
  (do 
    (get-new-opinions)
    (def irc (ref (make-irc-connection)))))

(defn reload [] (dosync (ref-set handle-line-ref handle-line)))
