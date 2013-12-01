(ns tree-edit-distance.core
  (:use [clojure.pprint]))

(defn init
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
  (cons a-tree
        (if (.hasChildNodes a-tree)
          (concat (tree-children a-tree)
                  (flatten (map tree-descendants (tree-children a-tree))))
          [])))

(defn rtdm-edit-distance
  "The RTDM algorithm for computing edit-distance.
   The trees are assumed to be org.w3c.dom.Documents"
  [tree-1 tree-2 thresh del-cost ins-cost sub-cost]
  (println "Tree1: " (.getNodeName tree-1))
  (println "Tree2: " (.getNodeName tree-2))
  (let [m (num-children tree-1)
        n (num-children tree-2)
        M (init m n)]
    (do
     (doseq [i (range m)
             j (range n)]
       
       (let [c1  (tree-children tree-1)
             c2  (tree-children tree-2)
             
             c-i (tree-descendants (nth c1 i))
             c-j (tree-descendants (nth c2 j))

             _   (println "Tree1-desc:" (count c-i) (.getNodeName (nth c1 i)))
             _   (println "Tree2-desc:" (count c-j) (.getNodeName (nth c2 j)))
             
             del (+ (aget M i (inc j))
                    (apply + (map del-cost c-i)))
             ins (+ (aget M (inc i) j)
                    (apply + (map ins-cost c-j)))

             s   (cond (> (aget M i j)
                          thresh)
                       Double/POSITIVE_INFINITY
                       
                       (.isEqualNode (nth c1 i)
                                     (nth c2 j))
                       0
                       
                       :else
                       (cond (not (.hasChildNodes (nth c1 i)))
                             (+ (sub-cost (nth c1 i) (nth c2 j))
                                (apply + (map ins-cost c-j)))

                             (not (.hasChildNodes (nth c2 j)))
                             (+ (sub-cost (nth c1 i) (nth c2 j))
                                (apply + (map del-cost c-i)))

                             :else
                             (rtdm-edit-distance (nth c1 i)
                                                 (nth c2 j)
                                                 thresh
                                                 del-cost
                                                 ins-cost
                                                 sub-cost)))]
         (println ins del s)
         (pprint M)
         (println :i (inc i) :j (inc j))
         (aset M (inc i) (inc j) (int (min del ins s)))
         (pprint M)))
     (aget M m n))))
