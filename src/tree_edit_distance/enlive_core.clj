(ns tree-edit-distance.enlive-core
  "A version of RTDM that operates on a tree produced by
   enlive."
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

(defn load-tree
  "Fetches a link's content and builds an enlive-tree with it"
  [a-link]
  (-> a-link
      (client/get)
      :body
      java.io.StringReader.
      html/html-resource))

(defn tree-edit-distance
  [tree1 tree2]
  0)

(defn tree-edit-distance-link
  "load trees and return their edit distance"
  [link1 link2]
  (tree-edit-distance (load-tree link1)
                      (load-tree link2)))
