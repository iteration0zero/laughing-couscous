(ns minimax.minimax
  (:gen-class)
  (:require [minimax.graph :as graph]))

(defn filter-alpha-beta [[idx node]]
  (or (nil? (:alpha node))
      (nil? (:beta node))
      (if (pos? (:player node))
        (<= (:beta node) (:alpha node))
        (<= (:alpha node) (:beta node)))))

(defn evaluate-alpha-beta [node]
  (let [e (graph/evaluate node)]
    (if (pos? (:player node))
      (assoc node :alpha e)
      (assoc node :beta e))))

(defn update-alpha-beta [g [parent-idx node]]
  (println "parent-idx: " parent-idx)
  (println "node: " node)
  (let [max-player (pos? (:player node))
        v-to-update (if max-player :alpha :beta)
        v (v-to-update node)
        comp-fn #(if max-player
                   (> %1 (or %2 -1))
                   (< %1 (or %2 1)))]
    (println max-player)
    (println v-to-update)
    (println v)
    (loop [g g
           [parent-idx node] [parent-idx node]
           v-comp (if parent-idx
                    (comp-fn v (get-in g [:nodes parent-idx v-to-update])))]
      (println "update alpha beta")
      (println "g")
      (println g)
      (if (and parent-idx v-comp)
        (recur
          (assoc-in g
                    [:nodes parent-idx v-to-update]
                    v)
          [(get-in g [:edges parent-idx]) (get-in g [:nodes parent-idx])]
          (if parent-idx
            (comp-fn v (get-in g [:nodes (get-in g [:edges parent-idx]) v-to-update]))))
        g))))

(defn expand [g]
  (transduce
        (comp
          (map (fn [idx]
                 [idx (get (:nodes g) idx)]))
          (filter filter-alpha-beta)
          (map (fn [[idx node]]
                 (let [children
                       (map (fn [child]
                              [idx child])
                            (graph/expand node))]
                   (if (not (empty? children))
                     children
                     [[idx node]]))))
          cat)
        (fn
          ([] {})
          ([g] g)
          ([g [parent-idx node]]
           (if (= (get (:nodes g) parent-idx) node)
             (update g :leaf-indices conj parent-idx)
             (let [node (evaluate-alpha-beta node)]
               (-> g
                   (update-alpha-beta [parent-idx node])
                   (update :nodes conj node)
                   (update :edges assoc (count (:nodes g)) parent-idx)
                   (update :leaf-indices conj (count (:nodes g))))))))
        (assoc g :leaf-indices [])
        (:leaf-indices g)))
