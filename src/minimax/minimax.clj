(ns minimax.minimax
  (:gen-class)
  (:require [minimax.graph :as graph]
            [tesser.core :as t]))

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
         new-g (assoc-in g
                         [:nodes current-idx :v]
                         (* (:player node)
                            children-max))]
     (if (and parent-idx
              (not= g new-g))
       (recur
         new-g
         parent-idx
         (get-in g [:edges :up parent-idx]))
       new-g))))

(defn expand [g]
  (->> (t/map (fn [idx]
                [idx (get (:nodes g) idx)]))
       (t/mapcat (fn [[idx node]]
                   (let [children
                          (doall
                            (map (fn [child]
                                   [idx (assoc child :v (graph/evaluate child))])
                                 (graph/expand node)))
                         max-eval-state (first (filter (fn [[idx child]]
                                                          (= 1.0
                                                             (* (:player node)
                                                                (:v child))))
                                                       children))]
                     (if max-eval-state
                       [max-eval-state]
                       children))))
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
