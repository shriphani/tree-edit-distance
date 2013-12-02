(ns tree-edit-distance.core
  (:use [clojure.pprint])
  (:import (org.htmlcleaner HtmlCleaner DomSerializer CleanerProperties)
           (org.w3c.dom Document)))

(defn init
  "Perform the correct initialization"
  [m n c1 c2 del-cost ins-cost]
  (let [M (make-array Integer/TYPE (inc m) (inc n))]
    (do
      (doseq [i (range (inc m))
              j (range (inc n))]
        (aset M i j (int
                     (+ (* del-cost c1)
                        (* ins-cost c2)))))
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
    []))

(declare rtdm-edit-distance)

(defn invert-cost
  [t1 t2 del-cost ins-cost sub-cost]
  (let [t1-desc (tree-descendants t1)
        t2-desc (tree-descendants t2)]
    (- (+ (* del-cost (count t1-desc))
          (* ins-cost (count t2-desc)))
       (rtdm-edit-distance t1 t2 del-cost ins-cost sub-cost))))

(defn rtdm-edit-distance
  "The RTDM algorithm for computing edit-distance.
   The trees are assumed to be org.w3c.dom.Documents"
  [tree-1 tree-2 del-cost ins-cost sub-cost]

  (let [m (num-children tree-1)
        n (num-children tree-2)

        t1-children (tree-children tree-1)
        t2-children (tree-children tree-2)
        
        t1-desc (tree-descendants tree-1)
        t2-desc (tree-descendants tree-2)

        M (init m n (count t1-desc) (count t2-desc) del-cost ins-cost)]
    
    (doseq [i (range m)
            j (range n)]
      (let [c-i (nth t1-children i)
            c-j (nth t2-children j)

            c-i-desc (tree-descendants c-i)
            c-j-desc (tree-descendants c-j)

            del (aget M i (inc j))
            ins (aget M (inc i) j)

            sub-i (- (aget M i j)
                     del-cost
                     ins-cost)

            sub (if (.isEqualNode c-i c-j)
                  (- sub-i
                     (* ins-cost (count c-j-desc))
                     (* del-cost (count c-i-desc)))
                  (cond
                   (or (not (.hasChildNodes c-i))
                       (not (.hasChildNodes c-j)))
                   (+ sub-i sub-cost)

                   (and (= (.getNodeName c-i) (.getNodeName c-j))
                        (= (.getNamedItem (.getAttributes c-i) "class")
                           (.getNamedItem (.getAttributes c-j) "class")))
                   (- sub-i (invert-cost c-i c-j del-cost ins-cost sub-cost))

                   :else
                   (+ sub-i sub-cost)))]
        (aset M (inc i) (inc j) (int (min del ins sub)))))
    (aget M m n)))

(defn rtdm-edit-distance-sim
  [tree-1 tree-2 del-cost ins-cost sub-cost]
  (let [t1-desc (tree-descendants tree-1)
        t2-desc (tree-descendants tree-2)]
    (- 1
       (/ (rtdm-edit-distance tree-1 tree-2 del-cost ins-cost sub-cost)
          (+ (* (+ (count t1-desc) 1) del-cost)
             (* (+ (count t2-desc) 1) sub-cost))))))

(defn get-xml-tree-body
  "Downloads a webpage and converts it to an org.w3.dom.Document"
  [page-src]
  
  (let [cleaner        (new HtmlCleaner)
        props          (.getProperties cleaner)
        cleaner-props  (new CleanerProperties)
        dom-serializer (new DomSerializer cleaner-props)
        tag-node       (.clean cleaner page-src)]
    
    (.createDOM dom-serializer tag-node)))

(defn rtdm-edit-distance-html
  [pg1 pg2 del-cost ins-cost sub-cost]
  (rtdm-edit-distance-sim
   (get-xml-tree-body pg1)
   (get-xml-tree-body pg2)
   del-cost
   ins-cost
   sub-cost))
