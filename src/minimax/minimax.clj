(ns minimax.minimax
  (:gen-class)
  (:require [minimax.graph :as graph]
            [tesser.core :as t]
            [clojure.core.reducers :as r]))

(defn propagate-v
  ([g current-idx]
   (propagate-v g current-idx (get-in g [:edges :up current-idx])))
  ([g current-idx parent-idx]
   (let [node (get-in g [:nodes current-idx])
         children (get-in g [:edges :down current-idx])
         children (if (coll? children)
                    children
                    [current-idx])
         children-v (->> children
                         (map (fn [c]
                                (:v (get-in g [:nodes c]))))
                         (map (partial *
                                       (get-in g [:nodes current-idx :player]))))
         children-max (apply max children-v)
         new-g (update-in g
                          [:nodes current-idx]
                          assoc
                          :v
                          (* (:player node)
                             children-max))]
     (if (and parent-idx
              (not= g new-g))
       (recur
         new-g
         parent-idx
         (get-in g [:edges :up parent-idx]))
       new-g))))

(defn add-node [g [parent-idx node]]
  (-> g
      (update :nodes conj node)
      (update-in [:edges :up]
                 assoc
                 (count (:nodes g))
                 parent-idx)
      (update-in [:edges :down]
                 update
                 parent-idx
                 (fn [c n] (conj (or c []) n))
                 (count (:nodes g)))
      (update :leaf-indices conj (count (:nodes g)))
      (propagate-v parent-idx)))

(defn expand [g]
  (let [leaf-indices-cutoff (min 20000
                                 (max (count (:leaf-indices g)) 0))
        g (assoc g :leaf-indices
                   (reduce (fn [c i]
                             (conj c i))
                           []
                           (range leaf-indices-cutoff (count (:leaf-indices g)))))
        leaf-indices (reduce (fn [c i]
                               (conj c i))
                             []
                             (range leaf-indices-cutoff))
        cf (fn
             ([] g)
             ([g1 g2]
              (let [added-nodes-indices (range (count (:nodes g))
                                               (count (:nodes g2)))
                    added-nodes-with-parent (doall (map (fn [n-idx]
                                                          [(get-in g2 [:edges :up n-idx])
                                                           (get-in g2 [:nodes n-idx])])
                                                        added-nodes-indices))]
                (reduce add-node
                        g1
                        added-nodes-with-parent))))
        g (->> leaf-indices
               (r/map (fn [idx]
                        [idx (get (:nodes g) idx)]))
               (r/mapcat (fn [[idx node]]
                            (let [children
                                   (doall
                                     (map (fn [child]
                                            [idx (assoc child :v (graph/evaluate child))])
                                          (graph/expand node)))]
                              children)))
               (r/map (partial add-node g))
               (r/fold cf))]
    g))


(comment
  g (transduce (comp (map (fn [idx]
                            [idx (get (:nodes g) idx)]))
                     (mapcat (fn [[idx node]]
                               (let [children
                                      (doall
                                        (map (fn [child]
                                               [idx (assoc child :v (graph/evaluate child))])
                                             (graph/expand node)))]
                                 children))))

               (assoc g :leaf-indices
                        (reduce (fn [c i]
                                  (conj c i))
                                []
                                (range leaf-indices-cutoff (count (:leaf-indices g)))))
               (assoc g :leaf-indices
                        (reduce (fn [c i]
                                  (conj c i))
                                []
                                (range leaf-indices-cutoff))))
    (->> (t/map (fn [idx]
                    [idx (get (:nodes g) idx)]))
         (t/mapcat (fn [[idx node]]
                     (let [children
                            (doall
                              (map (fn [child]
                                     [idx (assoc child :v (graph/evaluate child))])
                                   (graph/expand node)))]
                       children)))
         (t/fold {:reducer-identity (constantly (update g :leaf-indices subvec (min 10000
                                                                                    (max (count (:leaf-indices g)) 0))))
                  :reducer (fn
                              ([g] g)
                              ([g [parent-idx node]]
                               (-> g
                                   (update :nodes conj node)
                                   (update-in [:edges :up]
                                              assoc
                                              (count (:nodes g))
                                              parent-idx)
                                   (update-in [:edges :down]
                                              update
                                              parent-idx
                                              (fn [c n] (conj (or c []) n))
                                              (count (:nodes g)))
                                   (update :leaf-indices conj (count (:nodes g)))
                                   (propagate-v parent-idx))))
                  :combiner-identity (constantly (update g :leaf-indices subvec (min 10000
                                                                                     (max (count (:leaf-indices g)) 0))))
                  :combiner (fn [g1 g2]
                              (println "combining " (count (:nodes g1)) (count (:nodes g2)))
                              g2)})
         (t/tesser (t/chunk 512 (subvec (:leaf-indices g) 0 (min 10000
                                                                 (max (count (:leaf-indices g)) 0)))))))
