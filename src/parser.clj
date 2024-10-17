(ns parser
 (:require [instaparse.core :as insta]
           [clojure.string :as str]))

(def whitespace
  (insta/parser
   "whitespace = #'\\s+'"))

(def car-reg
  (insta/parser
   "Regulation                   = Headline Definitions Rules
    <Headline>                   = <'# '> <Word*>
    <Definitions>                = <'## Definitions'> DefinitionClause+
    <Rules>                      = <'## Rules'> RuleClause+

    ClauseNum = Integer <'. '>
    RuleClause = ClauseNum Rule+
    DefinitionClause = ClauseNum ForwardDeclaration+

    <ForwardDeclaration>         = DataAccess | NestedFwdDeclaration
    NestedFwdDeclaration         = DataAccess <' contains: '> NestedFwdDeclarationSubkey+
    <NestedFwdDeclarationSubkey> = <'- '> DataAccess

    Def                          = <'This regulation concerns '> DefSymbol <'.'>
    <Rule>                       = RuleExistence | RuleExistenceNum | RuleNumComparison

    RuleExistence                = DataAccess <' must have '> Symbol <'.'>
    RuleExistenceNum             = DataAccess <' must have '> Integer <' of '> Symbol+ <'.'>

    RuleNumComparison             = DataAccess <' must be '> NumComparison Integer <'.'>
    NumComparison                 = NCLT | NCEq | NCGT
    NCLT                          = <'less than '>
    NCEq                          = <'equal to '>
    NCGT                          = <'greater than '>

    DataAccess                    = Symbol | (Symbol SubKeyAccess)
    <SubKeyAccess>                = <'\\'s'> Symbol (SubKeyAccess)*
    Integer                       = #'-?\\d+'
    Word                          = #'[a-zA-Z]*'
    Symbol                        = #'\\*\\*[a-zA-Z -]*\\*\\*'
    <DefSymbol>                   = Symbol"
   :auto-whitespace whitespace))


;; Tokenizing

(def hanging-a #"\s*[Aa]\s+")
(def hanging-the #"\s*[Tt]he\s+")
(def hanging-and #"\s*[Aa]nd\s+")
(def hanging-also #"\s*[Aa]lso\s+")
(def multi-space #"\s{2,}")
(def comma #",")
(def code-comment #"_.*?_")

(def replacement-regexes
  [comma
   hanging-a
   hanging-the
   hanging-and
   hanging-also
   code-comment])

(defn cleanup
  [input-str]
  (let [cleaned-except-for-spacing
        (reduce #(str/replace %1 %2 " ")
                input-str
                replacement-regexes)]
    (str/replace cleaned-except-for-spacing multi-space " ")))

(defn parse-str
  [str]
  (-> str cleanup car-reg))

(defn parse-str-with-rule-tag
  [str start-tag]
  (-> str cleanup (car-reg :start start-tag)))

(defn tags-for-rule-string
  [start-tag rule-str]
  (parse-str-with-rule-tag rule-str start-tag))
(comment
  (tags-for-rule-string :RuleExistence "_This car is a car_ The **Car** must_must have_ have **antilock brakes**.")
  (tags-for-rule-string :RuleExistence "**Car** must have **Antilock brakes**."))