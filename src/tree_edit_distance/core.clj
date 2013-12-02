(ns tree-edit-distance.core
  (:use [clojure.pprint]))

(defn init
  "Perform the correct initialization"
  [m n]
  (let [M (make-array Integer/TYPE (inc m) (inc n))]
    (do
      (doseq [i (range (inc m))]
        (aset M i 0 (int 0)))
      (doseq [j (range (inc n))]
        (aset M 0 j (int 0)))
      M)))

(defn num-children
  "Expects a html tree"
  [a-tree]
  (if(.hasChildNodes a-tree)
    (.getLength (.getChildNodes a-tree)) 0))

(defn tree-children
  "Return level 1 children"
  [a-tree]
  (let [n  (num-children a-tree)
        cs (.getChildNodes a-tree)]
    (map
     #(.item cs %)
     (range n))))

(defn tree-descendants
  [a-tree]
  (if (.hasChildNodes a-tree)
    (concat (tree-children a-tree)
            (flatten (map tree-descendants (tree-children a-tree))))
    ['*]))

(defn rtdm-edit-distance
  "The RTDM algorithm for computing edit-distance.
   The trees are assumed to be org.w3c.dom.Documents"
  [tree-1 tree-2 thresh del-cost ins-cost sub-cost]
  '*)
