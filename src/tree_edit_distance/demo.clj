(ns tree-edit-distance.demo
  "Sets up a TED demo"
  (:require [clj-http.client :as client]
            [tree-edit-distance.core :as core])
  (:import (org.htmlcleaner HtmlCleaner DomSerializer CleanerProperties)
           (org.w3c.dom Document)))

(defn get-xml-tree-body-d
  "Downloads a webpage and converts it to an org.w3.dom.Document"
  [page-src]
  
  (let [cleaner        (new HtmlCleaner)
        props          (.getProperties cleaner)
        cleaner-props  (new CleanerProperties)
        dom-serializer (new DomSerializer cleaner-props)
        tag-node       (.clean cleaner page-src)]
    
    (.createDOM dom-serializer tag-node)))

(defn get-xml-tree-html
  [url]
  (-> url client/get :body get-xml-tree-body-d))
