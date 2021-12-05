(ns minimax.player
  (:require [minimax.graph :as graph]
            [minimax.minimax :as minimax]))

(defn make-move [g state-idx]
  (let [children
        (reduce (fn [acc [i1 i2]]
                  (if (= state-idx i2)
                    (conj acc (get (:nodes g) i1))
                    acc))
                []
                (:edges g))]
    (first (sort-by (fn [c]
                      (* (:player (get-in g [:nodes state-idx]))
                         (:v c)))
                    >
                    children))))

(defn expand-player-by [p n]
  (let [new-g (last (take n (iterate minimax/expand (:g p))))]
    {:g new-g
     :state-idx (:state-idx p)
     :move (make-move new-g (:state-idx p))}))

(defn move-player-to-state [p s]
  {:g (:g p)
   :state-idx s
   :move (make-move (:g p) s)})

(defn opp-move [p new-g-fn [y x]]
  (let [{:keys [g state-idx]} p
        new-state-idx
        (let [children
              (reduce (fn [acc [i1 i2]]
                        (if (= state-idx i2)
                          (conj acc [i1 (get (:nodes g) i1)])
                          acc))
                      []
                      (:edges g))]
          (first (first (filter #(= (:last-move (second %)) [y x]) children))))]
    (if new-state-idx
      {:g g
       :state-idx new-state-idx
       :move (make-move g new-state-idx)}
      (new-g-fn (get-in g [:nodes state-idx]) [y x]))))

(defn get-player [g]
  (let [player (agent {:g g
                       :state-idx 0
                       :move (make-move g 0)})]
    player))
