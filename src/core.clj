(ns core
  (:require [parser]
           	[clause-res]))

(def bare-env-map
  {:errors [] :data nil})


(defn brindle-eval
  [text input-data]
	 (let [parsed (parser/parse-str text)
      	 env (atom (assoc bare-env-map :data input-data))]
  	 (clause-res/resolve-regulation env {} parsed)))