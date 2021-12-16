(ns minimax.core
  (:gen-class)
  (:require [minimax.graph :as graph :refer [state evaluate]]
            [minimax.minimax :as minimax]
            [minimax.player :as player]
            [uncomplicate.fluokitten.core :refer [fmap]]
            [tangle.core :as tangle]
            [clojure.core.async :as async]
            [clojure.core.reducers :as r]
            [tesser.core :as t]
            [uncomplicate.neanderthal.native :refer [dv dge]])
  (:use [uncomplicate.neanderthal core math])
  (:use [uncomplicate.fluokitten core jvm]))

(defrecord SampleMinimaxNode [board player last-move])

(extend SampleMinimaxNode
        graph/IState
        {:state
          (fn [this]
            {:player (:player this)
             :board (:board this)})})

(def TicTacToeNode SampleMinimaxNode)

(defn n-in-a-row-state-extension [n t]
  (extend t
          graph/IGraphNode
          {:expand
            (fn [this]
              (->> (doall (for [y (range n)
                                x (range n)]
                            [y x]))
                   (filter (fn [[y x]]
                             (= 0 (get-in (state this) [:board y x]))))
                   (fmap (fn [[y x]]
                           (->SampleMinimaxNode
                             (:board (assoc-in (state this) [:board y x] (:player (state this))))
                             (* (:player (state this)) -1)
                             [y x]))))
              #_(into []
                      (comp (filter (fn [[y x]]
                                      (= 0 (get-in (state this) [:board y x]))))
                            (map (fn [[y x]]
                                   (->SampleMinimaxNode
                                    (:board (assoc-in (state this) [:board y x] (:player (state this))))
                                    (* (:player (state this)) -1)
                                    [y x]))))))
           :is-terminal?
            (fn [this]
              (or
                (= 1.0 (Math/abs (evaluate this)))
                (let [board-size (count (:board (state this)))
                      board-matrix (dge board-size board-size (:board (state this)))]
                  (= (* board-size board-size 1.0)
                     (sum (fmap abs board-matrix))))))}))

(defn get-anti-diagonal-for-dim [dim]
  (dge dim dim
      (for [n (range dim)]
        (assoc (into [] (repeat dim 0)) (- dim 1 n) 1))
      {:layout :row}))

(defn get-submatrices [m sm-dim]
  (fmap (fn [[y x]]
          (submatrix m y x (+ y sm-dim) (+ x sm-dim)))
       (for [y (range (inc (- (mrows m) sm-dim)))
             x (range (inc (- (ncols m) sm-dim)))]
         [y x])))

(defn n-in-a-row-state-evaluation [n t]
  (extend t
          graph/IEvaluable
          {:evaluate
            (fn [this]
              (let [
                    board-size (count (:board (state this)))
                    board-matrix (dge board-size board-size (:board (state this)))
                    board-submatrices (get-submatrices board-matrix n)
                    row-v (dv (repeat n 1))
                    column-vs (fmap dv (for [m (range n)]
                                         (assoc (into [] (repeat n 0)) (- n 1 m) 1)))
                    anti-diagonal (get-anti-diagonal-for-dim n)
                    submatrix-checks (fmap (fn [board-submatrix]
                                             (let [row-v-m-v (mv board-submatrix row-v)
                                                   row-v-tm-v (mv (trans board-submatrix) row-v)
                                                   row-checks (fmap dot (repeat row-v-m-v) column-vs)
                                                   col-checks (fmap dot (repeat row-v-tm-v) column-vs)
                                                   diag-check (sum (dia board-submatrix))
                                                   anti-diag-check (sum (dia (mm anti-diagonal board-submatrix)))
                                                   check-v (or (first (filter #(>= (Math/abs %) n)
                                                                              (concat row-checks col-checks [diag-check anti-diag-check])))
                                                               0)]
                                               (/ check-v
                                                  n)))
                                          board-submatrices)]
                  (first (sort-by #(Math/abs %) >
                                  submatrix-checks))))}))


(n-in-a-row-state-extension 3 TicTacToeNode)
(n-in-a-row-state-evaluation 3 TicTacToeNode)

(def tictactoe-root (->SampleMinimaxNode
                      [[0 0 0]
                       [0 0 0]
                       [0 0 0]]
                      1
                      nil))

(def sample-g
  {:nodes [tictactoe-root]
   :edges {}
   :vs {0 -1}
   :leaf-indices [0]})

(def test-g
  {:nodes [(->SampleMinimaxNode
             [[-1 1 -1]
              [1 1 -1]
              [0 0 1]]
             -1
             nil)]

   :edges {}
   :vs {0 1}
   :leaf-indices [0]})

(comment
  (->> (t/fold {:combiner (fn
                            ([] [])
                            ([r] (println "r: " r) r)
                            ([r1 r2] (println "r1: " r1)
                                     (println "r2: " r2)
                             r2))
                :reducer (fn
                           ([] [])
                           ([acc n]
                            (conj acc (inc n))))})
       (t/tesser (t/chunk 512 (range 1000))))
  (def player (player/get-player sample-g))
  (def player (player/get-player sample-expanded-g))
  (def player (player/get-player (read-string (slurp "resources/ttt_minimax.edn"))))
  (deref player)
  (minimax/expand sample-g)
  (async/thread
    (dotimes [n 1]
             (swap! player player/expand-player-by 5))
    (println "done!"))
  (conj :a :b)
  (async/thread (swap! player player/opp-move
                             (fn [n [y x]]
                               {:g {:nodes [(-> n
                                                (update :board assoc-in [y x] (:player n))
                                                (update :player * -1))]
                                    :edges {}
                                    :leaf-indices [0]}
                                :state-idx 0
                                :move nil})
                             [1 0])
                (println "done!"))
  (async/thread (swap! player assoc :state-idx 0)
                (println "done!"))
  (async/thread (swap! player player/make-move)
                (println "done!"))
  (get-in @player [:g :nodes (:state-idx @player) :board])
  (get @player :g)
  (spit "resources/ttt_minimax.edn" (pr-str sample-expanded-g))
  (map (fn [c-idx] (get-in @player [:g :nodes c-idx]))
       (get-in @player [:g :edges :down (:state-idx @player)]))
  (get-in @player [:g :edges :down])
  (get-in @player [:g :nodes (:state-idx @player)])
  (count (get-in @player [:g :nodes]))
  (minimax/expand test-g)
  (last (take 10 (iterate minimax/expand sample-expanded-g)))
  (last (take 5 (iterate minimax/expand-1 sample-expanded-g)))
  (apply max (map identity nil))
  (minimax/expand sample-expanded-g)
  (minimax/expand-1 sample-expanded-g)
  (def sample-expanded-g *1)
  (count (:leaf-indices sample-expanded-g))
  (let [first-n-nodes (map (fn [i]
                             [i (get (:nodes sample-expanded-g) i)])
                           (range 10))
        first-n-nodes (filter #(not (nil? (second %)))
                              first-n-nodes)]
    (viz/view-graph first-n-nodes
                    (fn [[i n]]
                      [(get-in sample-expanded-g [:edges :down i])
                       (get-in sample-expanded-g [:nodes
                                                  (get-in sample-expanded-g [:edges :down i])])])
                    :node->descriptor (fn [[i n]] {:label (apply str
                                                                 (into [(:player n) "\n"]
                                                                       (concat
                                                                         (interleave (:board n) (repeat "\n"))
                                                                         [(:v n)])))})))
  (tangle/dot->svg gdot)
  (vec #{:a :b})
  (def i (tangle/dot->image gdot "png"))
  (tangle/dot->image gdot "png")
  (javax.imageio.ImageIO/read i)
  (viz/view-image (javax.imageio.ImageIO/read i))
  gdot
  (get-in sample-expanded-g [:nodes 0])
  (filter #(graph/is-terminal? %)
          (:nodes sample-expanded-g))
  (graph/evaluate (get-in sample-expanded-g [:nodes 11100]))
  (graph/is-terminal? (get-in sample-expanded-g [:nodes 11100]))
  (Math/abs 0)

  (conj nil 5)
  (= (* 3 3) 9.0)
  (subvec (into [] (range 10000))

          (min 10000
               (max (count (into [] (range 10000))) 0)))

  (def x (dv 1 1 1))

  (def e1 (dv 1 0 0))
  (def e2 (dv 0 1 0))
  (def e3 (dv 0 0 1))

  (def a (dge 3 3 [-1 -1 0 0 -1 0 -1 0 -1]
              {:layout :row}))

  (sum (fmap abs a))
  (mv a x)

  (dot (mv a x) e3)

  (sum (dia (mm (get-anti-diagonal-for-dim 3) a)))
  (sum (dia a))

  (def anti-diagonal (dge 2 2 [[0 1] [1 0]] {:layout :row}))

  (clojure.pprint/pprint (get-anti-diagonal-for-dim 100))

  (dot (mv (trans a) x) e3))
