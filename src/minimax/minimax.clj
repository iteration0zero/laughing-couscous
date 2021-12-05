(ns minimax.minimax
  (:gen-class)
  (:require [minimax.graph :as graph]
            [tesser.core :as t]))

(defn propagate-v [g [parent-idx node]]
  (let [v (:v node)]
    (loop [g g
           [parent-idx node] [parent-idx node]]
      (if (and parent-idx
               (> (* (get-in g [:nodes parent-idx :player])
                     v)
                  (* (get-in g [:nodes parent-idx :player])
                     (get-in g [:nodes parent-idx :v]))))
        (recur
          (assoc-in g
                    [:nodes parent-idx :v]
                    v)
          [(get-in g [:edges parent-idx]) (get-in g [:nodes parent-idx])])
        g))))

(defn expand [g]
  (->> (t/map (fn [idx]
                [idx (get (:nodes g) idx)]))
       (t/filter (fn [[idx node]]
                   (not (graph/is-terminal? node))))
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
                                 (propagate-v [parent-idx node])
                                 (update :nodes conj node)
                                 (update :edges assoc (count (:nodes g)) parent-idx)
                                 (update :leaf-indices conj (count (:nodes g))))))
                :combiner-identity (constantly (update g :leaf-indices subvec (min 10000
                                                                                   (max (count (:leaf-indices g)) 0))))
                :combiner (fn [_ g2]
                            g2)})
       (t/tesser (t/chunk 512 (subvec (:leaf-indices g) 0 (min 10000
                                                               (max (count (:leaf-indices g)) 0)))))))
