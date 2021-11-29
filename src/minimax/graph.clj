(ns minimax.graph
  (:gen-class))

(defrecord MinimaxNode [])

(defprotocol IGraphNode
  (expand [this])
  (is-terminal? [this]))

(defprotocol IState
  (state [this]))

(defprotocol IEvaluable
  (evaluate [this]))

(defn minimax-graph-node? [node]
  (every? #(satisfies? % node)
          IGraphNode
          IState
          IEvaluable))

(comment
  (let [test (reify IState (state [_] :test))]
    (macroexpand-1 '(reify IState (state [_] :test)))
    (macroexpand-1 '(reify* [minimax.graph.IState] (state [_] :test)))))
