(ns tree-edit-distance.core)

(defn init
  [m n]
  (let [M (make-array Integer/TYPE m n)]
    (do
      (doseq [i (range m)]
        (aset M i 0 (int 0)))
      (doseq [j (range n)]
        (aset M 0 j (int 0)))
      M)))

(defn rtdm-edit-distance
  "The RTDM algorithm for computing edit-distance.
   The trees are assumed to be org.w3c.dom.Documents"
  [tree-1 tree-2 thresh]

  (let [tree1-children (.getChildNodes tree-1)
        tree2-children (.getChildNodes tree-2)

        m              (.getLength tree1-children)
        n              (.getLength tree2-children)
        M              (init m n)]
    M))
