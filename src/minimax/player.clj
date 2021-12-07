(ns minimax.player
  (:require [minimax.graph :as graph]
            [minimax.minimax :as minimax]))

(defn make-move [g state-idx]
  (let [children-indices (get-in g [:edges :down state-idx])
        children (map (fn [c-idx]
                        (get-in g [:nodes c-idx]))
                      children-indices)]
    (println "make move children: " children)
    (println (first (sort-by (fn [c]
                               (* (:player (get-in g [:nodes state-idx]))
                                  (:v c)))
                             >
                             children)))
    (first (sort-by (fn [c]
                      (* (:player (get-in g [:nodes state-idx]))
                         (:v c)))
                    >
                    children))))

(defn expand-player-by [p n]
  (let [new-g (last (take n (iterate minimax/expand (:g p))))]
    (assoc p
           :g new-g
           :state-idx (:state-idx p)
           :move (make-move new-g (:state-idx p)))))

(defn move-player-to-state [p s]
  (assoc p
         :g (:g p)
         :state-idx s
         :move (make-move (:g p) s)))

(defn opp-move [p new-g-fn [y x]]
  (println "opp move")
  (let [{:keys [g state-idx]} p
        new-state-idx
        (let [children-indices (get-in g [:edges :down state-idx])]
          (first (filter (fn [c-idx]
                           (let [c (get-in g [:nodes c-idx])]
                             (= (:last-move c) [y x])))
                         children-indices)))]
    (if new-state-idx
      (assoc p
             :g g
             :state-idx new-state-idx
             :move (make-move g new-state-idx))
      (new-g-fn (get-in g [:nodes state-idx]) [y x]))))

(defn get-player [g]
  (let [player (agent {:g g
                       :state-idx 0
                       :move (make-move g 0)})]
    player))
