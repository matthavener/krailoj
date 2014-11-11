(ns krailoj.core
  (:require [clj-http.client :as http-client]
            [clojure.data.csv :as csv]
            [irclj.core :as irclj]
            [clojure.string :as string]))

(defn
  seconds-now
  []
   (int (/ (System/currentTimeMillis) 1000)))

(def csv-url "https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=txt")

(defn 
  get-new-opinions 
  "Download and parse a new opinions file. This can block."
  [] 
  (let [f ((http-client/get csv-url) :body) ; get the csv
        hcf (map #(string/split % #"\t") (string/split f #"\n"))
        cf (rest hcf)] ; transform to vector of vectors
    (map (fn [l] { ; build the regex and response strings, using the entire line as a "key"
                  ; the (?i) appears make it case insensitive
                  :line l
                  :match (re-pattern (str "(?i)" "(\\(|\\s|^)(" (first l) ")(\\s|\\)|\\.|\\?|\\!|$)")) 
                  :response (str "? " (string/join " " (rest l)))
                  }) 
         cf)))

(defn 
  match-responses-for-line 
  "Return a list of responses for a given line, or empty list for no response"
  [opinions l]
  (reduce 
    (fn [responses opinion] 
      (if-let [grp (re-find (opinion :match) l)] 
        (conj responses {
                         ; include the matching string from the regex in the
                         ; response
                         :response (str (nth grp 2) (opinion :response))
                         :line (opinion :line)
                         })
        responses)) 
    '() 
    opinions
    ))

(def cooldown-secs 86400)

(defn
  responses-for-privmsg
  "Returns a list of irc messages to send for a private message in a channel"
  [opinions last-fired user chan said]
  (let [matches (match-responses-for-line opinions said)
        ; debounc responses, default to seconds of 0 if last-fired is empty
        responses (filter #(> (seconds-now) (get last-fired (% :line) 0)) matches)]
    { 
     :last-fired (zipmap (map #(% :line) responses) (repeat (+ cooldown-secs (seconds-now))))
     :messages (map #(str user ": " (% :response)) responses) 
    }))

; global state

; map of lines to last-sent time
(def last-fired (ref {}))

; ref to opinions vector of opinion maps { :match :response }
(def opinions (ref []))

; functions with side effects (on global state)

(defn 
  set-new-opinions 
  [] 
  (dosync 
    (ref-set opinions (get-new-opinions)) ; get the new opinions
    (alter last-fired select-keys (map #(% :line) @opinions)) ; remove anything from last-fired thats been removed from opinions
    :ok))

(defn 
  handle-line 
  "Handle a line from irclj"
  [irc msg]
  (let [user (msg :nick)
        chan (first (msg :params))
        said (apply str (rest (msg :params)))]
    (if (.contains said "I CALL UPON THE POWER OF THE SPREADSHEET")
      (do ; reload spreadsheet 
        (println "reloading...")
        (irclj/message irc chan "loading hacking tools...")
        (future (do
                  (set-new-opinions)
                  (println "got " (count @opinions) " opinions")
                  (irclj/message irc chan (str "loaded " (count @opinions) " hacks")))))
      (let
       [responses (responses-for-privmsg @opinions @last-fired user chan said)]
       ; update the last-fired time for all the responses
       (when-not (empty? responses)
           (dosync
            (alter last-fired merge (responses :last-fired)))
           ; send each response out to irc
           (doseq [res (responses :messages)] 
                  (irclj/message irc chan res))))
      )))

(def handle-line-ref (ref handle-line))

(defn 
  make-irc-connection 
  "Create the irclj connection/irc var"
  []
  (irclj/connect "irc.madhax.net" 6667 "krailOS" :real-name "krailOS-clojure" :mode 0 :callbacks {:privmsg (fn [& v] (apply @handle-line-ref v))}))

(defn 
  -main 
  "Main function"
  [& args]
  (do 
    (set-new-opinions)
    (def irc (ref (make-irc-connection)))
    (irclj/join @irc "#main")))

; just for debugging.. better way?
(defn reload [] (dosync (ref-set handle-line-ref handle-line)))
