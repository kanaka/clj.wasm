(ns spec-extract
  (:require ["fs" :as fs]
            ["unified" :refer [unified]]
            ["@unified-latex/unified-latex-util-parse" :refer [unifiedLatexAstCompiler
                                                              unifiedLatexFromStringMinimal]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]))

(prn :here1)

(defn extract-math-blocks [content]
  (let [math-pattern #"(?s)\.\. math::\s*\n\s*(.+?)(?=\n\n|\Z)"
        matches (re-seq math-pattern content)]
    (map second matches)))

(defn clean-math-text [text]
  (-> text
      (str/replace #"^\s+" "")  ; Remove leading whitespace
      (str/replace #"\n\s+" "\n")  ; Normalize indentation
      (str/trim)))

(defn find-production-name [content]
  (let [prod-node (some #(when (and (= (:type %) "macro")
                                   (= (:content %) "production")) %) content)]
    (when prod-node
      (let [group (first (filter #(= (:type %) "group")
                                (drop-while #(not= % prod-node) content)))]
        (when group
          (:content (first (filter #(= (:type %) "string") (:content group)))))))))

(defn find-symbol [content]
  (some #(when (and (= (:type %) "macro")
                    (.startsWith (:content %) "T")) 
           (:content %))
        content))

(defn find-rules [content]
  (let [nodes (:content (first content))]
    (->> nodes
         (partition-by #(= (:content %) "\\\\"))  ; Split on row separators
         (filter #(some (fn [n] (and (= (:type n) "macro")
                                    (= (:content n) "text"))) %))
         (map (fn [row]
                (let [text-node (first (filter #(and (= (:type %) "macro")
                                                    (= (:content %) "text")) row))
                      rule-text (get-in text-node [:content 0 :content 0 :content])
                      target (->> row
                                (drop-while #(not= (:content %) "Rightarrow"))
                                rest
                                (take-while #(not= (:type %) "\\"))
                                (map :content)
                                (remove nil?)
                                (str/join ""))]
                  (str "'" rule-text "' (* => " target " *)")))))))

(defn extract-production [ast]
  (try
    (extract-production ast)
    (catch :default e
      {:error (.-message e)})))

(defn parse-latex [parser math-text]
  (try
    (let [cleaned-text (clean-math-text math-text)
          ast (.parse parser cleaned-text)
          parsed-ast (js->clj ast :keywordize-keys true)]
      {:success true
       :ast parsed-ast
       :production (extract-production parsed-ast)})
    (catch :default e
      {:success false
       :error (.-message e)
       :text math-text})))

(defn process-file [filepath]
  (println "\nProcessing:" filepath)
  (let [content (fs/readFileSync filepath "utf-8")
        math-blocks (extract-math-blocks content)
        parser (-> (unified.)
                   (.use unifiedLatexFromStringMinimal #js {:mode "math"}))]
    (when-let [block (first math-blocks)]
      (println "\n=== Math Block ===")
      (println "Raw content:")
      (println block)
      (let [result (parse-latex parser block)
            ;; Remove :position (:source, :start, :end)
            result (walk/postwalk #(if (map? %) (dissoc % :position) %) result)]
        (pprint result)
        (if (:success result)
          (let [{:keys [name symbol rules]} (:production result)]
            (println "\nEBNF Production:")
            (when (and name rules)
              (println (str name " ::= " (str/join "\n  | " rules)))))
          (do
            (println "\nParsing failed:")
            (println (:error result))))))))

(defn -main [& args]
  (let [filepath (or (first args) "spec/document/core/text/types.rst")]
    (process-file filepath)))

(-main)
