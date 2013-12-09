(ns tree-edit-distance.demo
  "Sets up a TED demo"
  (:require [clj-http.client :as client]
            [tree-edit-distance.core :as core])
  (:import (org.htmlcleaner HtmlCleaner DomSerializer CleanerProperties)
           (org.w3c.dom Document)))

(defn forum-index-page-similarity-test
  []
  (let [base-index  "http://forums.finalgear.com/finalgear-com-news/"
        alt-indices ["http://forums.finalgear.com/the-site-itself/"
                     "http://forums.finalgear.com/the-forums/"
                     "http://forums.finalgear.com/video-offers/"
                     "http://forums.finalgear.com/top-gear-america/"]]))
