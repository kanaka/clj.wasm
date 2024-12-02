(ns spec-extract
  (:require ["fs" :as fs]
            ["unified" :refer [unified]]
            ["@unified-latex/unified-latex-util-parse" :refer [unifiedLatexAstCompiler
                                                              unifiedLatexFromStringMinimal]]
            [clojure.string :as S]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]))

;;
;; parser
;;

(defn split-match-n
  "Takes a predicate and a collection. The pred function takes
  a collection and returns the number of items matching at head of
  collection (or true/false where true = 1, false/0 = no match).
  Returns:
  [items-before-match matching-items items-after-match]"
  [pred coll]
  (loop [xs coll, before []]
    (if (empty? xs)
      [(seq before) nil nil]
      (let [n (pred xs)]
        (if (or (nil? n) (false? n) (zero? n))
          (recur (rest xs) (conj before (first xs)))
          (let [n (if (number? n) n 1)]
            [(seq before) (seq (take n xs)) (seq (drop n xs))]))))))

(defn production-name? [[{:keys [type content]} & xs]]
  (and (= type "macro") (= content "production")))

(defn production-symbol? [[{:keys [type content]} & xs]]
  (and (= type "macro") (.startsWith content "T")))


(defn strs= [nodes & strs]
  (let [nodes (take (count strs) nodes)]
    (and (every? #(= "string" (:type %)) nodes)
         (= strs (map :content nodes)))))

(defn production-op? [xs]
  (cond
    (strs= xs "&" ":" ":" "=" "&") 5
    (strs= xs "&" ":" ":" "=")     4
    (strs= xs ":" ":" "=" "&")     4
    (strs= xs ":" ":" "=")         3
    :else nil))

(defn production-alt? [xs]
  (cond
    (strs= xs "&" "&" "|" "&") 4
    (strs= xs "&" "|" "&")     3
    (strs= xs "|" "&")         2
    (strs= xs "&" "|")         2
    (strs= xs "|")             1
    :else nil))


(defn right-arrow? [[{:keys [type content]} & xs]]
  (and (= type "macro") (= content "Rightarrow")))

(defn newline? [[{:keys [type content]} & xs]]
  (and (= type "macro") (= content "\\")))

(defn find-rule [nodes]
  (let [[rule-full alt post] (split-match-n production-alt? nodes)
        [rule _ right] (split-match-n right-arrow? rule-full)]
    [{:rule rule
      :metadata right} post]))

(defn find-rules [nodes]
  (loop [rules []
         nodes nodes]
    (if (empty? nodes)
      rules
      (let [[rule nodes] (find-rule nodes)]
        (recur (conj rules rule) nodes)))))


(defn parse-production [nodes]
  (try
    (let [[_ [psym] nodes] (split-match-n production-symbol? nodes)
          [_ _ nodes] (split-match-n production-op? nodes)
          rules (find-rules nodes)]
      {:symbol psym
       :rules rules})
    (catch :default e
      {:error (.-message e)})))

(defn parse-productions [ast]
  (loop [productions []
         nodes (get-in ast [:content 0 :content])]
    (if (empty? nodes)
      productions
      (let [[_      [pname1] nodes1] (split-match-n production-name? nodes)
            [pnodes [pname2] nodes2] (split-match-n production-name? nodes1)
            prod (parse-production pnodes)]
        (recur (conj productions (assoc prod :name pname1))
               (if pname2 (concat [pname2] nodes2) []))))))

;;
;; emitter
;;

(declare emit-nodes)

(defn emit-node [{:as node :keys [type content]} right?]
  (condp = type
    "whitespace" " "
    "string"     (condp = content
                   "&" " "
                   "~" " "
                   content)
    "group"      (emit-nodes content right?)
    "macro"      (if (= "\\" content)
                   " "
                   (if right?
                     content
                     (if (.startsWith content "T")
                       (subs content 1)
                       (throw (js/Error. (str "unknown left side macro '" content "'"))))))))

(defn emit-nodes [nodes right?]
  (loop [result ""
         nodes nodes]
    (if (empty? nodes)
      result
      (let [[node & nodes] nodes]
        (if (and (= "macro" (:type node)) (= "text" (:content node)))
          (recur (str result "'" (emit-node (first nodes) right?) "'")
                 (rest nodes))
          (recur (str result (emit-node node right?))
                 nodes))))))

(defn emit-production [{:keys [name symbol rules]}]
  (let [pname (emit-node symbol false)
        plen (count pname)
        rule-delim (str "\n" (apply str (repeat plen " ")) " |   ")]
    (str pname " ::= "
         (S/join rule-delim
                 (for [{:keys [rule metadata]} rules]
                   (str (emit-nodes rule false)
                        "(* => "
                        (emit-nodes metadata true)
                        " *)"))))))

(defn parse-latex [parser math-text]
  (try
    (let [ast (.parse parser math-text)
          parsed-ast (js->clj ast :keywordize-keys true)]
      {:success true
       :ast parsed-ast})
    (catch :default e
      {:success false
       :error (.-message e)
       :text math-text})))

;;
;;
;;

(defn clean-math-text [text]
  (-> text
      (S/replace #"^\s+" "")  ; Remove leading whitespace
      (S/replace #"\n\s+" "\n")  ; Normalize indentation
      (S/trim)))

(defn extract-math-blocks [content]
  (let [math-pattern #"(?s)\.\. math::\s*\n\s*(.+?)(?=\n\n|\Z)"]
    (->> (re-seq math-pattern content)
         (map second)
         (map clean-math-text))))

(defn process-file [filepath]
  (println "\nProcessing:" filepath)
  (let [content (fs/readFileSync filepath "utf-8")
        math-blocks (extract-math-blocks content)
        parser (-> (unified.)
                   (.use unifiedLatexFromStringMinimal #js {:mode "math"}))]
    ;;(doseq [block (drop 3 (take 4 math-blocks))]
    (doseq [block (drop 4 (take 5 math-blocks))]
      (println "\n=== Math Block ===")
      (println "Raw content:")
      (println block)
      (let [result (parse-latex parser block)]
        (if (:success result)
          ;; Remove :position (:source, :start, :end)
          (let [ast (walk/postwalk #(if (map? %) (dissoc % :position) %) (:ast result))
                productions (parse-productions ast)]
            (pprint ast)
            (doseq [production productions]
              (pprint production)
              (println "\nEBNF Production:")
              (println (emit-production production))))
          (do
            (println "\nParsing failed:")
            (println (:error result))))))))

(defn -main [& args]
  (let [filepath (or (first args) "spec/document/core/text/types.rst")]
    (process-file filepath)))

(-main)
