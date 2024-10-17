(ns clause-res
  (:require [parser :refer [car-reg
                            parse-str-with-rule-tag
                            tags-for-rule-string]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(declare resolve-clause-vec)

(def test-env
  {:errors []
   :car {:driver-side-airbag           true
         :antilock-brakes              true
         :electronic-stability-control true
         :rear-view-camera             true
         :crash-rating                 5
         :second-row                   {:airbags        "I have airbags"
                                        :shoulder_belts false}}})



;; Default environment

(defn make-error-record
  [ctx clause-vec]
  (println "error record for " clause-vec)
  {:clause-num (:clause-num ctx)
   :section (:section ctx)
   :clause-vec clause-vec})

(defn make-warning-record
  [ctx clause-vec val]
  {:clause-num (:clause-num ctx)
   :section (:section ctx)
   :clause-vec clause-vec
   :value val})

(defn false-with-env-error
  [env ctx clause-vec]
  (swap! env assoc :errors (cons (make-error-record ctx clause-vec)
                                 (:errors @env)))
  false)

(defn pass-value-with-env-warning
  [env ctx clause-vec val]
  (swap! env assoc :warnings (cons (make-warning-record ctx clause-vec val)
                                  (:warnings @env)))
  val)


(defn resolve-symbol
  [_env _ctx [_symbol-key keystr]]
  (-> keystr
      (str/replace "**" "")
      (str/replace " " "-")
      str/lower-case
      keyword))
(comment
  (resolve-symbol test-env 0 [:Symbol "**Car**"]))

(defn resolve-data-access
  [env ctx [_tag head & rest :as clause-vec]]
  (let [envmap (:data @env)
        retrieved (if (empty? rest)
                    (get envmap (resolve-clause-vec env ctx head))
                    (get-in envmap (map #(resolve-clause-vec env ctx %) (cons head rest))))]
    (if retrieved retrieved (pass-value-with-env-warning env ctx clause-vec retrieved))))
(comment
  (resolve-data-access test-env 0 (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-data-access test-env 0 (tags-for-rule-string :DataAccess "**Car**'s **second row**"))
  (tags-for-rule-string :DataAccess "**Car**'s **second row**"))

(defn resolve-rule-existence
  [env ctx [_tag data-access search-key :as clause-vec]]
  (let [res (get (resolve-clause-vec env ctx data-access)
                 (resolve-clause-vec env ctx search-key))]
    (if res res (false-with-env-error env ctx clause-vec))))
(comment
  (resolve-rule-existence test-env 0 [:RuleExistence [:DataAccess [:Symbol "**Car**"] [:Symbol "**second row**"]] [:Symbol "**airbags**"]])
  (resolve-rule-existence test-env 0 (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**."))
  (resolve-rule-existence test-env 0 (tags-for-rule-string :RuleExistence "The **Car**'s **second row** must have **airbags**.")))

(defn resolve-num-comparison
  [env ctx [_tag [comparison] :as clause-vec]]
  (match comparison
    :NCLT <
    :NCEq =
    :NCGT >
    :else (false-with-env-error env ctx clause-vec)))

(defn resolve-integer
  [env ctx [_ int-str :as clause-vec]]
  (let [res (parse-long int-str)]
    (if res
      res
      (false-with-env-error env ctx clause-vec))))

(defn resolve-rule-num-comparison
  "Numeric comparison of two values, the first being data."
  [env ctx [_ data-access comparison num :as clause-vec]]
  (println "got to rule-num-comparison with clause " clause-vec)
  (let [comparison-result
        ((resolve-clause-vec env ctx comparison)
         (resolve-clause-vec env ctx data-access)
         (resolve-clause-vec env ctx num))]
    (if comparison-result
      true
      (false-with-env-error env ctx clause-vec))))
(comment
  (let [vec
        [:RuleNumComparison [:DataAccess [:Symbol "**Car**"] [:Symbol "**crash rating**"]] [:NumComparison [:NCGT]] [:Integer "2"]]
        at (atom {:data {:car {:crash-rating 2}}})
        ctx {:clause-num 1
             :section :rules}]
    (resolve-rule-num-comparison at ctx vec)
    @at)
  (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3.")
  (resolve-rule-num-comparison test-env 0 (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3."))
  )

(defn truthy?
  [x]
  (not (or (nil? x) (false? x))))
(comment
  (truthy? 3)
  (truthy? nil)
  (truthy? false))

(defn resolve-rule-existence-num
  [env ctx [_tag data-access num & access-key-symbols :as this-vec]]
  (let [data-to-access (resolve-data-access env ctx data-access)
        result (->> access-key-symbols
                    (map #(resolve-symbol env ctx %))
                    (map #(get data-to-access %))
                    (filter truthy?)
                    count
                    (<= (resolve-integer env ctx num)))]
    (if result
      true
      (false-with-env-error env ctx this-vec))))
(comment
  (resolve-rule-existence-num test-env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **superlock brakes**, **electronic stability control**, and **drivers side airbag**."))
  (resolve-rule-existence-num test-env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **antilock brakes**, **electronic stability control**, and **drivers side airbag**.")))

(defn resolve-forward-declaration
  [env ctx [_tag & data-accesses]]
  (let [check-declaration (fn [acc x]
                            (if
                             (nil? (resolve-clause-vec env ctx x))
                             (do (false-with-env-error env ctx x) acc)
                             acc))]
    (reduce check-declaration env data-accesses)))

(comment
  (tags-for-rule-string :Definitions "## Definitions
                                          **Car** _is defined as a vehicle with 4 wheels._
                                          **Hello** _hello_")
  (tags-for-rule-string :Definitions "## Definitions A **Car** _is defined as a vehnicle with 4 wheels._")
  (resolve-forward-declaration test-env {}
                               (tags-for-rule-string :Definitions "## Definitions
                                          **Car**_is defined as a vehicle with 4 wheels._
                                          **Hello** ")))
(defn upsplice-data-access
  [top-access bottom-access]
  (vec (concat [:DataAccess]
               (rest top-access)
               (rest bottom-access))))

(defn resolve-nested-forward-declaration
  [env ctx [_tag top-access & rest]]
  ;; top-access is a data access representing a map,
  ;; and rest is a seq of data accesses representing keys of that map.
  ;; for each of rest, make a data access with top-access at the front (using concat)
  ;; then feed it into resolve-forward-declaration.
  (let [data-accesses (map #(upsplice-data-access top-access %) rest)]
    (map #(resolve-clause-vec env ctx %) data-accesses)))
(comment
  (resolve-clause-vec test-env 0
                      (upsplice-data-access
                       (tags-for-rule-string :DataAccess "**Car**")
                       (tags-for-rule-string :DataAccess "**rear view camera**")))
  (resolve-nested-forward-declaration test-env 0
                                      (tags-for-rule-string :NestedFwdDeclaration "A **Car** contains:
                                          - **crash rating**
                                          - **rear view camera**"))
  (resolve-nested-forward-declaration test-env 0
                                      [:NestedFwdDeclaration
                                       (tags-for-rule-string :DataAccess "**Car**")
                                       (tags-for-rule-string :DataAccess "**crash rating**")
                                       (tags-for-rule-string :DataAccess "**rear view camera**")]))

(defn resolve-clause-num
  [env ctx [_tag int-clause]]
  (resolve-clause-vec env ctx int-clause))

(defn resolve-numbered-rule-or-definition-clause
  [env ctx [_tag clause-num-vec & rest]]
  (let [new-ctx (assoc ctx :clause-num (resolve-clause-vec env nil clause-num-vec))]
    (doall (map #(resolve-clause-vec env new-ctx %) rest))))

(defn resolve-numbered-definition
  [env ctx vec]
  (resolve-numbered-rule-or-definition-clause env (assoc ctx :section :definitions) vec))

(defn resolve-numbered-rule
  [env ctx vec]
  (resolve-numbered-rule-or-definition-clause env (assoc ctx :section :rules) vec))


(defn resolve-regulation
  [env ctx [_tag & rest]]
  (->> rest
       (map #(resolve-clause-vec env ctx %))
       doall)
  @env)


(defn resolve-clause-vec
  "The first element of the clause vec is a keyword denoting its parse rule.
  Every parse rule corresponds to a resolve-[clause-name] fn."
  [env ctx clause]
  (println "parsing " clause)
  ((match (first clause)
  	 :Regulation           resolve-regulation
     :ForwardDeclaration   resolve-forward-declaration
     :NestedFwdDeclaration resolve-nested-forward-declaration
     :ClauseNum            resolve-clause-num
     :DefinitionClause     resolve-numbered-definition
     :RuleClause           resolve-numbered-rule
     :Symbol               resolve-symbol
     :Integer              resolve-integer
     :NumComparison        resolve-num-comparison
     :DataAccess           resolve-data-access
     :RuleNumComparison    resolve-rule-num-comparison
     :RuleExistence        resolve-rule-existence
     :RuleExistenceNum     resolve-rule-existence-num)
   env ctx clause))
(comment
  (resolve-clause-vec test-env {} [:Symbol "**Car**"])
  (resolve-clause-vec test-env {} (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-clause-vec test-env {} (tags-for-rule-string :DataAccess "**Car**'s **second row**")))