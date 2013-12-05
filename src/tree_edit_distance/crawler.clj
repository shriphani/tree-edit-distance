(ns tree-edit-distance.crawler
  "A generic crawl framework"
  (:require [clj-http.client :as client]
            [tree-edit-distance.core :as core]
            [net.cgrand.enlive-html :as html]
            (org.bovinegenius [exploding-fish :as uri]))
  (:use     [clojure.set :as clj-set]))

(defn extract-links
  [url page-src]
  (let [a-tags (html/select
                (html/html-resource (java.io.StringReader. page-src))
                [:a])]
    (distinct
     (map
      (fn [l]
        (uri/fragment l nil))
      (filter
       #(and (identity %) (= (uri/host url) (uri/host %)))
       (map
        #(try (uri/resolve-uri url (-> % :attrs :href))
              (catch Exception e nil))
        a-tags))))))

(defn crawl
  ([entry target-pg-src thresh limit]
     (crawl entry target-pg-src thresh limit [{:src nil :target entry}] {} (set [])))
  
  ([entry target-pg-src thresh limit queue pages visited]
     (if (<= limit (count visited))
       {:pages pages
        :visited visited}
       (let [new-link (-> queue first :target)
             _        (println :link new-link)
             body     (-> new-link client/get :body)
             links    (filter
                       #(and %
                             (not= new-link %)
                             (not (some #{%} (set (map (fn [v] (-> v :target)) visited)))))
                       (extract-links new-link body))]
         (if body
           (do
             (Thread/sleep 1500)
             (recur entry
                    target-pg-src
                    thresh
                    limit
                    (rest (concat queue (map
                                         (fn [l]
                                           {:src new-link
                                            :target l})
                                         links)))
                    (if (>= (core/rtdm-edit-distance-html
                             target-pg-src body 1 1 1)
                            thresh)
                      (conj pages {:url new-link :body body})
                      pages)
                    (conj visited (first queue))))
           (do
             (Thread/sleep 1500)
             (recur entry
                    target-pg-src
                    thresh
                    limit
                    (rest queue)
                    pages
                    (conj visited (first queue)))))))))

(defn build-tpm
  "Builds a TPM graph
   Args:
    entry: entry page
    target-example: a link that contain an example of a target pg
    thresh: a threshold value
    limit: number of pages to look at"
  [entry target-example thresh limit]
  (let [target-pg-src (-> target-example client/get :body)]
    (crawl entry target-pg-src thresh limit)))

(defn rtdm-edit-distance-urls
  [url1 url2]
  (let [body1 (-> url1 client/get :body)
        body2 (-> url2 client/get :body)]
    (core/rtdm-edit-distance-html body1 body2 1 1 1)))
