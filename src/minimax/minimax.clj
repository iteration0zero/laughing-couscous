(ns minimax.minimax
  (:gen-class)
  (:use [uncomplicate.fluokitten core jvm])
  (:require [minimax.graph :as graph]
            [tesser.core :as t]
            [clojure.core.reducers :as r]))

(defn get-affected-node-idxs
  ([g current-idxs]
   (let [current-idxs
         (->> (t/filter #(not (nil? %)))
              (t/fold {:reducer conj
                       :reducer-identity (constantly [])

                       :combiner (comp distinct into)
                       :combiner-identity (constantly [])})

              (t/tesser (t/chunk 512 current-idxs)))
         [cs ps]
         (->> (t/map (fn [c-idx]
                       [c-idx (get-in g [:edges :up c-idx])]))
              (t/fold {:reducer (fn [[cs ps] [c p]]
                                  [(conj cs c)
                                   (conj ps p)])
                       :reducer-identity (constantly [[] []])
                       :combiner (fn [[cs ps] [acs aps]]
                                   [(into cs acs)
                                    (into ps aps)])
                       :combiner-identity (constantly [[] []])})
              (t/tesser (t/chunk 512 current-idxs)))
         ps (->> (t/filter #(not (nil? %)))
                 (t/fold {:reducer (comp distinct conj)
                          :reducer-identity (constantly [])
                          :combiner (comp distinct into)
                          :combiner-identity (constantly [])})
                 (t/tesser (t/chunk 512 ps)))
         ps (if (not (empty? ps))
              (get-affected-node-idxs g ps)
              [])]
     (into cs
           ps))))

(defn calculate-new-vs
  [g current-idxs]
  (->> (t/map (fn [current-idx]
                (let [node (get-in g [:nodes current-idx])
                      children (get-in g [:edges :down current-idx])
                      children (if (coll? children)
                                 children
                                 [current-idx])
                      children-max (->> children
                                        (fmap (fn [c]
                                                (or (get-in g [:vs c])
                                                    (get-in g [:nodes c :v]))))
                                        (fmap (partial *
                                                       (get-in g [:nodes current-idx :player])))
                                        (apply max))]
                   {current-idx children-max})))
       (t/fold {:reducer merge
                :reducer-identity (constantly {})
                :combiner merge
                :combiner-identity (constantly {})})
       (t/tesser (t/chunk 512 current-idxs))))

(defn get-new-edges
  [new-nodes new-node-idxs]
  (->> (t/map (fn [idxidx]
                (let [node-idx (get new-node-idxs idxidx)
                      parent-idx (first (get new-nodes idxidx))]
                  {:up {node-idx parent-idx}
                   :down {parent-idx [node-idx]}})))
       (t/fold {:reducer conj
                :reducer-identity (constantly [])
                :combiner into
                :combiner-identity (constantly [])})
       (t/tesser (t/chunk 512 (range (count new-node-idxs))))))

(defn expand [g]
  (let [leaf-indices-cutoff (min 20000
                                 (count (:leaf-indices g)))
        leaf-indices (subvec (:leaf-indices g) 0 leaf-indices-cutoff)
        g (update g :leaf-indices
                   subvec
                   leaf-indices-cutoff)
        new-nodes (->> (t/map (fn [idx]
                                [idx (get (:nodes g) idx)]))
                       (t/filter #(not (graph/is-terminal? (second %))))
                       (t/map (fn
                               ([[idx node]]
                                (let [children
                                       (fmap (fn [child]
                                               [idx (assoc child :v (graph/evaluate child))])
                                             (graph/expand node))
                                      max-eval-state (first (filter #(= 1.0
                                                                        (* (:v (second %))
                                                                           (:player node)))
                                                                    children))]
                                  (if max-eval-state
                                    [max-eval-state]
                                    children)))))
                       (t/fold {:reducer into
                                :reducer-identity (constantly [])
                                :combiner into
                                :combiner-identity (constantly [])})
                       (t/tesser (t/chunk 512 leaf-indices)))
         new-node-idxs (into []
                             (range (count (:nodes g))
                                    (+ (count (:nodes g))
                                       (count new-nodes))))
         new-edges (get-new-edges new-nodes
                                  new-node-idxs)
         new-nodes-nodes (->> (t/map second)
                              (t/fold {:reducer conj
                                       :reducer-identity (constantly [])
                                       :combiner into
                                       :combiner-identity (constantly [])})
                              (t/tesser (t/chunk 512 new-nodes)))
         g (update g :nodes into new-nodes-nodes)
         new-edges (->> (t/fold {:reducer (->> into
                                               (partial merge-with)
                                               (partial merge-with))
                                 :reducer-identity (constantly {})
                                 :combiner (->> into
                                                (partial merge-with)
                                                (partial merge-with))
                                 :combiner-identity (constantly (:edges g))})
                        (t/tesser (t/chunk 512 new-edges)))
         g (assoc g :edges new-edges)
         node-vs (->> (t/map (fn [node-idx]
                               {node-idx (get-in g [:nodes node-idx :v])}))
                      (t/fold {:reducer merge
                               :reducer-identity (constantly {})
                               :combiner merge
                               :combiner-identity (constantly {})})
                      (t/tesser (t/chunk 512 new-node-idxs)))
         new-nodes-parent-idxs (->> (t/map first)
                                    (t/fold {:reducer conj
                                             :reducer-identity (constantly [])
                                             :combiner into
                                             :combiner-identity (constantly [])})
                                    (t/tesser (t/chunk 512 new-nodes)))
         affected-node-idxs (get-affected-node-idxs g new-nodes-parent-idxs)
         new-vs (calculate-new-vs g affected-node-idxs)
         g (update g
                   :vs
                   merge
                   node-vs
                   new-vs)
         g (update g
                    :leaf-indices
                    into
                    new-node-idxs)]
     g))

(comment)
