(ns minimax.minimax
  (:gen-class)
  (:require [minimax.graph :as graph]))

(defn expand [g]
  (transduce
        (comp
          (map (fn [idx]
                 [idx (get (:nodes g) idx)]))
          (map (fn [[idx node]]
                 (map (fn [child]
                        [idx child])
                      (graph/expand node))))
          cat)
        (fn
          ([] {})
          ([g] g)
          ([g [parent-idx node]]
           (-> g
               (update :nodes conj node)
               (update :edges conj #{parent-idx (count (:nodes g))}))))
        (assoc g :leaf-index (count (:nodes g)))
        (range (:leaf-index g) (count (:nodes g)))))
