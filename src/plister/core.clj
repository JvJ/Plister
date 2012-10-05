(ns plister.core
  (:require (clojure
             [string :as str]
             [xml :as xml]
             [pprint :as ppr])
            (clojure.java
             [io :as io]))
  (:gen-class))

(defn map-to-plist
  [m]
  (cond
   (string? m) {:tag :string :content [(str m)]}
   (integer? m) {:tag :integer :content [(str m)]}
   (number? m) {:tag :number :content [(str m)]}
   (vector? m) {:tag :array
                :content (map map-to-plist m)}
   (map? m) {:tag :dict
             :content
             (->>
              (for [[k v] m]
                [{:tag :key :content [(name k)]}
                 (map-to-plist v)])
              (apply concat))
             }))

(defn make-plist
  [m]
  {:tag :plist
   :attrs {:version "1.0"}
   :content [(map-to-plist m)]})

(defn maybe-kw
  [x]
  (if (re-find #"[ \t\n\,]+" x)
    x
    (keyword x)))

(defn plist-to-map
  "Takes an xml map and turns it into a plist."
  [xmap]
  (let [tag (:tag xmap)
        content (:content xmap)]
    (cond
     (= tag :dict) (apply hash-map (map plist-to-map content))
     (= tag :array) (vec (map plist-to-map content))
     (= tag :key) (maybe-kw (str/trim (first content)))
     (= tag :integer) (read-string (first content))
     (= tag :float) (read-string (first content))
     (= tag :string) (str/trim (first content))
     (= tag :plist) (first (map plist-to-map content)); TODO: Is this a hack?
     :else nil)))

(defn -main
  [& filenames]
  (doseq [f filenames]
    (let [[newfile read-func write-func trans-func]
          (cond (re-find #"\.clj" f)
                [(str/replace f #"\.clj" ".plist") load-file xml/emit make-plist]
                (re-find #"\.plist|\.xml" f)
                [(str/replace f #"\.plist|\.xml" ".clj") xml/parse ppr/pprint plist-to-map]
                )]
      (println newfile)
      (->>
       (read-func f)
       (trans-func)
       (#(with-out-str (write-func %)))
       (spit newfile)))))