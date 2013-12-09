(ns tree-edit-distance.url
  "Contains the URL components in the Vidal et. al paper"
  (:require (org.bovinegenius [exploding-fish :as uri])))

(def *num-levels-match* 2)

(defn url-sim
  [url1 url2]
  (let [p1 (filter
            #(not (empty? %))
            (-> url1 uri/path (clojure.string/split #"/")))
        p2 (filter
            #(not (empty? %))
            (-> url2 uri/path (clojure.string/split #"/")))]

    (and (= (count p1)
            (count p2))
         (>= *num-levels-match*
             (-
              (count p1)
              (count
               (map (fn [[u v]] (= u v)) (map vector p1 p2))))))))

(defn url-pattern
  "Generate a url pattern using a regex tree. The resulting
   URL looks like a perl-regex."
  [urls]
  (reduce
   (fn [pattern url]
     (map [k (range )]))
   ""
   urls))
