(ns minimax.core
  (:gen-class)
  (:require [minimax.graph :as graph :refer [state evaluate]]
            [minimax.minimax :as minimax]
            [uncomplicate.fluokitten.core :refer [fmap]])
  (:use [uncomplicate.neanderthal core native math]))

(defrecord SampleMinimaxNode [board player])

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
              (into []
                    (comp (filter (fn [[y x]]
                                    (= 0 (get-in (state this) [:board y x]))))
                          (map (fn [[y x]]
                                 (->SampleMinimaxNode
                                  (:board (assoc-in (state this) [:board y x] (:player (state this))))
                                  (* (:player (state this)) -1)))))
                    (for [y (range n)
                          x (range n)]
                      [y x])))
           :is-terminal?
            (fn [this]
              (or
                (= 1 (Math/abs (evaluate this)))
                (let [board-size (count (:board (state this)))
                      board-matrix (dge board-size board-size (:board (state this)))]
                  (= (* board-size board-size)
                     (sum (fmap abs board-matrix))))))}))

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
                    column-vs (doall (map dv (for [m (range n)]
                                               (assoc (into [] (repeat n 0)) (- n 1 m) 1))))
                    anti-diagonal (get-anti-diagonal-for-dim n)
                    submatrix-checks
                    (doall (map (fn [board-submatrix]
                                  (let [row-v-m-v (mv board-submatrix row-v)
                                        row-v-tm-v (mv (trans board-submatrix) row-v)
                                        row-checks (map dot (repeat row-v-m-v) column-vs)
                                        col-checks (map dot (repeat row-v-tm-v) column-vs)
                                        diag-check (sum (dia board-submatrix))
                                        anti-diag-check (sum (dia (mm anti-diagonal board-submatrix)))
                                        check-v (some #(>= n (Math/abs %))
                                                      (concat row-checks col-checks [diag-check anti-diag-check]))]
                                     (/ check-v
                                        (max (Math/abs check-v)) 1)))
                                board-submatrices))]
                 (or (some #(>= n (Math/abs %))
                           submatrix-checks)
                     0)))}))

(n-in-a-row-state-extension 3 TicTacToeNode)
(n-in-a-row-state-evaluation 3 TicTacToeNode)

(def tictactoe-root (->SampleMinimaxNode
                      [[0 0 0]
                       [0 0 0]
                       [0 0 0]]
                      1))

(defn get-submatrices [m sm-dim]
  (doall
    (map (fn [[y x]]
           (submatrix a y x (+ y sm-dim) (+ x sm-dim)))
         (for [y (range (- (mrows m) sm-dim))
               x (range (- (ncols m) sm-dim))]
           [y x]))))

(def sample-g
  {:nodes [tictactoe-root]
   :edges #{}
   :leaf-index 0})

(minimax/expand sample-g)

(minimax/expand *1)

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

(defn get-anti-diagonal-for-dim [dim]
  (dge dim dim
      (for [n (range dim)]
        (assoc (into [] (repeat dim 0)) (- dim 1 n) 1))
      {:layout :row}))

(clojure.pprint/pprint (get-anti-diagonal-for-dim 100))

(dot (mv (trans a) x) e3)
