(ns tree-edit-distance.enlive-core
  "A version of RTDM that operates on a tree produced by
   enlive."
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

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

(defn load-tree
  "Fetches a link's content and builds an enlive-tree with it"
  [a-link]
  (->> a-link
       client/get
       :body
       java.io.StringReader.
       html/html-resource
       (filter (fn [x] (:tag x))) ; pick out the tree and not the docstring
       first))

(defn tree-children
  [a-tree]
  (->> a-tree :content (filter map?)))

(defn num-children
  [a-tree]
  (-> a-tree tree-children count))

(defn tree-descendants
  [a-tree]
  (if (-> a-tree tree-children seq)
    (+ (num-children a-tree)
       (apply + (map tree-descendants (tree-children a-tree))))
    0))

(declare tree-edit-distance)

(defn invert-cost
  [tree1 tree2 del-cost ins-cost sub-cost]
  (let [t1-desc (tree-descendants tree1)
        t2-desc (tree-descendants tree2)]
    (- (+ (* del-cost t1-desc)
          (* ins-cost t2-desc))
       (tree-edit-distance tree1 tree2 del-cost ins-cost sub-cost))))

(defn tree-edit-distance
  [tree1 tree2 del-cost ins-cost sub-cost]
  (let [m (num-children tree1)
        n (num-children tree2)

        t1-children (tree-children tree1)
        t2-children (tree-children tree2)

        t1-desc (tree-descendants tree1)
        t2-desc (tree-descendants tree2)

        M (init m n t1-desc t2-desc del-cost ins-cost)]
    
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

            sub (if (= c-i c-j)
                  (- sub-i
                     (* ins-cost c-j-desc)
                     (* del-cost c-i-desc))
                  (cond
                   (or (not (-> c-i :content (filter map?)))
                       (not (-> c-j :content (filter map?))))
                   (+ sub-i sub-cost)

                   (or (= (-> c-i :tag) (-> c-j :tag)))
                   (- sub-i (invert-cost c-i c-j del-cost ins-cost sub-cost))

                   :else
                   (+ sub-i sub-cost)))]
        (aset M (inc i) (inc j) (int (min del ins sub)))))
    (aget M m n)))

(defn tree-edit-distance-link
  "load trees and return their edit distance"
  [link1 link2]
  (let [t1 (load-tree link1)
        t2 (load-tree link2)]
    (- 1
       (/ (tree-edit-distance t1 t2 1 1 1)
          (+ (tree-descendants t1)
             (tree-descendants t2))))))
