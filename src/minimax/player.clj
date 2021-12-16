(ns minimax.player
  (:require [minimax.graph :as graph]
            [minimax.minimax :as minimax]))

(defn make-move [{:keys [g state-idx] :as p}]
  (let [children-indices (get-in g [:edges :down state-idx])]
    (assoc p
           :g (:g p)
           :state-idx
           (first (sort-by (fn [c-idx]
                             (* (:player (get-in g [:nodes state-idx]))
                                (get-in g [:vs c-idx])))
                           >
                           children-indices)))))

(defn expand-player-by [p n]
  (let [new-g (last (take n (iterate minimax/expand (:g p))))]
    (assoc p
           :g new-g
           :state-idx (:state-idx p))))

(defn move-player-to-state [p s]
  (assoc p
         :g (:g p)
         :state-idx s))

(defn opp-move [p new-g-fn [y x]]
  (let [{:keys [g state-idx]} p
        new-state-idx
        (let [children-indices (get-in g [:edges :down state-idx])]
          (first (filter (fn [c-idx]
                           (let [c (get-in g [:nodes c-idx])]
                             (= (:last-move c) [y x])))
                         children-indices)))]
    (if new-state-idx
      (assoc p
             :state-idx new-state-idx)
      (new-g-fn (get-in g [:nodes state-idx]) [y x]))))

(defn get-player [g]
  (let [player (atom {:g g
                      :state-idx 0})]
    player))
