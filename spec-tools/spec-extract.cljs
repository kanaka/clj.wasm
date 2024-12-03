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

(defn production-name? [[[t c] & xs]]
  (and (= t "macro") (= c "production") 2))

#_(defn production-symbol? [[[t1 c1] [t2 c2] & xs]]
  ;; ["macro" "Tinstr"] ["string" "_I"] ["whitespace" nil] ["string" "&"]
  (cond
    (and (= t1 "macro") (re-seq #"^T" c1)
         (= t2 "string") (= c2 "_I"))
    2
    
    (and (= t1 "macro") (re-seq #"^T" c1))
    1

    :else
    0))

(defn production-symbol? [[[t1 c1] & xs]]
  (and (= t1 "macro") (re-seq #"^T" c1)))


(defn strs= [nodes & strs]
  (let [nodes (take (count strs) nodes)]
    (and (every? #(= "string" %) (map first nodes))
         (= strs (map second nodes)))))

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


(defn right-arrow? [[[t c] & xs]]
  (and (= t "macro") (= c "Rightarrow")))

(defn newline? [[[t c] & xs]]
  (and (= t "macro") (= c "\\")))

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
         nodes ast]
    (if (empty? nodes)
      productions
      (let [[_      [_ pname1] nodes1] (split-match-n production-name? nodes)
            [pnodes [_ pname2] nodes2] (split-match-n production-name? nodes1)
            prod (parse-production pnodes)]
        (recur (conj productions (assoc prod :name pname1))
               (if pname2 (concat [pname2] nodes2) []))))))

(declare emit-nodes)

(defn split-productions [ast]
  (loop [productions []
         nodes ast]
    (if (empty? nodes)
      productions
      (let [[_      [m1 p1] nodes1] (split-match-n production-name? nodes)
            [pnodes [m2 p2] nodes2] (split-match-n production-name? nodes1)
            pname1 (emit-nodes [p1] true)]
        (recur (conj productions [pname1 pnodes])
               (if p2 (concat [m2 p2] nodes2) []))))))

;;
;; emitter
;;


(defn emit-nodes [nodes meta?]
;;  (prn :emit-nodes (count nodes) :meta? meta?)
  (loop [result ""
         nodes nodes]
    (if (empty? nodes)
      result
      (let [[[t1 c1 :as n1] [t2 c2 :as n2] [t3 c3 :as n3] [t4 c4 :as n4]
             [t5 c5 :as n5] [t6 c6 :as n6] [t7 c7 :as n7] [t8 c8 :as n8]] nodes]
;;        (prn :meta? meta? :t1 t1 :c1 c1)
        (cond
          (and (= ["macro" "text"] n1)
               (= "group" t2)
               (= 1 (count c2))
               (= "string" (ffirst c2)))
          (recur (str result "'" (-> c2 first second) "'") (drop 2 nodes))


          ;; remove prefixes
          ;; ["string" "I"] ["string" "'"] ["group" [["string" ":"]]] ["macro" "Tlabel"]
          (and (= ["string" "I"] n1)
               (= ["string" "'"] n2)
               (= ["group" [["string" ":"]]] n3)
               (= "macro" t4) (re-seq #"^T" c4))
          (recur (str result (subs c4 1)) (drop 4 nodes))

          (and (= "string" t1)
               (= ["group" [["string" ":"]]] n2)
               (= "macro" t3) (re-seq #"^T" c3))
          (recur (str result (subs c3 1)) (drop 3 nodes))
          
          #_(and (= "string" t1)
               (= ["group" [["string" ":"]]] n2)
               (= ["macro" "Tu"] n3)
               (= "string" t4))
          #_(recur (str result "u" c4) (drop 4 nodes))

          ;; ["macro" "X"] ["group" [["string" "in"]]] ["string" "_1"] ["group" [["string" ":"]]] ["macro" "Tinstr"]
          (and (= ["macro" "X"] n1)
               (= ["group" [["string" "in"]]] n2)
               (= "string" t3) (re-seq #"^_[0-9][0-9]*" c3)
               (= ["group" [["string" ":"]]] n4)
               (= "macro" t5) (re-seq #"^T" c5))
          (recur (str result (subs c5 1)) (drop 5 nodes))

          ;; ["string" "l^"] ["macro" "ast"] ["group" [["string" ":"]]]
          (and (= "string" t1) (re-seq #"^[a-z]\^" c1)
               (= ["macro" "ast"] n2)
               (= ["group" [["string" ":"]]] n3))
          (recur result (drop 3 nodes))


          ;; prefix followed by an identifier
          ;; ["macro" "X"] ["group" [["string" "in"]]] ["group" [["string" ":"]]] ["macro" "Tplaininstr"]
          ;; ["macro" "X"] ["group" [["string" "bt"]]] ["group" [["string" ":"]]] ["macro" "Tblocktype"]
          (and (= ["macro" "X"] n1)
               (= "group" t2) (= "string" (ffirst c2))
               (= ["group" [["string" ":"]]] n3)
               (= "macro" t4) (re-seq #"^T" c4))
          (recur (str result (subs c4 1)) (drop 4 nodes))

          (and (= ["string" "t"] n1)
               (= "macro" t3) (re-seq #"^T" c3))
          (recur (str result (subs c3 1)) (drop 3 nodes))

          ;; ["string" "y"] ["string" ","] ["string" "I"] ["string" "'"] ["group" [["string" ":"]]] ["macro" "Ttypeuse"] ["string" "_I"]
          (and (= ["string" "y"] n1)
               (= ["string" ","] n2)
               (= ["string" "I"] n3)
               (= ["string" "'"] n4)
               (= ["group" [["string" ":"]]] n5)
               (= "macro" t6) (re-seq #"^T" c6)
               (= ["string" "_I"] n7))
          (recur (str result (subs c6 1)) (drop 7 nodes))



          ;; repeats/vec
          (and (= "string" t1) (re-seq #"^t_[0-9]" c1)
               (= ["macro" "ast"] n2)
               (= ["macro" "Tvec"] n4)
               (= ["string" "("] n5)
               (= "macro" t6) (re-seq #"^T" c6)
               (= ["string" ")"] n7))
          (recur (str result "(" (subs c6 1) ")*") (drop 7 nodes))

          ;; ["macro" "Tvec"] ["string" "("] ["macro" "Tlabelidx"] ["string" "_I"] ["string" ")"]
          (and (= ["macro" "Tvec"] n1)
               (= ["string" "("] n2)
               (= "macro" t3) (re-seq #"^T" c3)
               (= ["string" "_I"] n4)
               (= ["string" ")"] n5))
          (recur (str result "(" (subs c3 1) ")*") (drop 5 nodes))

          (= ["string" "("] n1)
          (recur (str result "( ") (drop 1 nodes))

          (and (= ["string" ")"] n1)
               (= ["string" "^"] n2)
               (= ["macro" "ast"] n3))
          (recur (str result " )*") (drop 3 nodes))


          ;; optional
          ;; ["macro" "Tid"] ["string" "_1^"] ["string" "?"]
          ;; ["macro" "Tid"] ["string" "_2^"] ["string" "?"]
          (and (= "macro" t1) (re-seq #"^T" c1)
               (or (= ["string" "^"] n2)
                   (and (= "string" t2) (re-seq #"^_[0-9][0-9]*\^$" c2)))
               (= ["string" "?"] n3))
          (recur (str result "(" (subs c1 1) ")?") (drop 3 nodes))


          (= ["macro" "\\"] n1)
          (recur (str result " ") (drop 1 nodes))


          ;; ignore indexes
          (and (= ["string" "_"] n1)
               (= ["group" [["string" "I"] ["string" "'"]]] n2))
          (recur result (drop 2 nodes))

          (= ["string" "_I"] n1)
          (recur result (drop 1 nodes))


          (= ["macro" "epsilon"] n1)
          (recur (str result "'' ") (drop 1 nodes))

          ;; meta/right side no whitespace fills
          meta?
          (if (= "group" t1)
            (recur (str result (emit-nodes c1 true)) (drop 1 nodes))
            (recur (str result c1) (drop 1 nodes)))


          ;; whitespace
          (= ["whitespace" nil] n1)
          (recur (str result " ' '* ") (drop 1 nodes))

          (and (= ["string" "~"] n1)
               (= ["string" "~"] n2))
          (recur (str result " ' '* ") (drop 2 nodes))

          (= ["string" "~"] n1)
          (recur (str result " ' '* ") (drop 1 nodes))

          (= ["macro" "qquad"] n1)
          (recur (str result "  ") (drop 1 nodes))

          (= ["string" "&"] n1)
          (recur (str result " ") (drop 1 nodes))

          (and (not meta?)
               (= ["string" "t"] n1))
          (recur (str result " ' '* ") (drop 1 nodes))


          ;; One off workarounds
          ;; ["macro" "text"] ["group" [["string" "br"] ["macro" "_"] ["string" "if"]]]
          (and (= ["macro" "text"] n1)
               (= "group" t2)
               (= "string" (get-in c2 [0 0]))
               (= "macro" (get-in c2 [1 0]))
               (= "string" (get-in c2 [2 0])))
          (recur (str result " " (get-in c2 [0 1]) (get-in c2 [1 1]) (get-in c2 [2 1]))
                 (drop 2 nodes))


          :else
          (let [unmatched (take 10 nodes)
                cont (if (> (count nodes) 10) "..." "")]
            (throw (js/Error. (str "unmatched nodes: \n" unmatched cont)))))))))

(defn emit-production [symbol rules]
  (let [pname (subs (second symbol) 1) ;; TODO:
        plen (count pname)
        rule-delim (str "\n" (apply str (repeat plen " ")) " |   ")]
    (str pname " ::= "
         (S/join rule-delim
                 (for [{:keys [rule metadata]} rules]
                   (str (emit-nodes rule false)
                        "(* => "
                        (emit-nodes metadata true)
                        " *)"))))))

(defn parse-latex [parser math-text verbose?]
  (try
    (let [ast (.parse parser math-text)
          parsed-ast (js->clj ast :keywordize-keys true)]
      {:success true
       :unified-ast ast
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
  (let [math-pattern #"(?s)\.\. math::\s*\n\s*(.+?)(?=\n\n|$)"]
    (->> (re-seq math-pattern content)
         (map second)
         (map clean-math-text))))

(defn load-files [files verbose?]
  (let [ast-parser (-> (unified.)
                       (.use unifiedLatexFromStringMinimal #js {:mode "math"}))]
    (for [file files
          :let [_ (when verbose?
                    (println "Processing:" file))
                content (fs/readFileSync file "utf-8")]
          block (extract-math-blocks content)
          :let [full-ast (parse-latex ast-parser block)
                ast (walk/postwalk
                      #(if (map? %) [(:type %) (:content %)] %)
                      (get-in full-ast [:ast :content 0 :content]))]
          [name ast] (split-productions ast)]
      {:file file :block block
       :name name :ast ast})))

(defn list-rst-files [dir]
  (let [files (.readdirSync fs dir)]
    (->> files
         (filter #(.isFile (.statSync fs (str dir "/" %))))
         (map #(str dir "/" %)))))


(defn -main [& args]
  (let [dir (or (first *command-line-args*) ".")
        verbose? true
        files (->> (list-rst-files dir)
                   ;;(filter #(re-seq #"instructions\.rst" %)))
                   (filter #(re-seq #"types\.rst" %)))
        raw-productions (load-files files verbose?)
        productions (reduce
                      (fn [ps {:keys [file block name ast] :as p}]
                        (try
                          (let [{:keys [symbol rules]} (parse-production ast)
                                ebnf (emit-production symbol rules)]
                            ;; TODO: if name already exists
                            (conj ps [name (merge p {:symbol symbol
                                                     :ast ast
                                                     :ebnf ebnf})]))
                          (catch :default e
                            (conj ps [name (merge p {:error e})]))))
                      [] raw-productions)]
    (println "\nEBNF Productions:")
    (doseq [[name {:keys [file block error symbol ast ebnf]}] productions]
      (if error
        (do
          (println "\n\n===== Error =====")
          (println "== Location ==")
          (println (str "file: " file ", production name: " name))
          (when verbose?
            (println "== Raw Math Block ==")
            (println block)
            (println "== Production AST ==")
            (println ast))
          (println "== Error Message ==")
          (println error)
          (js/process.exit 1))
        (do
          (println (str "(* " file ":"
                        " production name: " name
                        ", symbol: " symbol " *)"))
          (println ebnf))))))

(-main)


