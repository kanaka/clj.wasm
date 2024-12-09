(ns spec-extract
  (:require ["fs" :as fs]
            ["unified" :refer [unified]]
            ["@unified-latex/unified-latex-util-parse" :refer [unifiedLatexAstCompiler
                                                              unifiedLatexFromStringMinimal]]
            [clojure.string :as S]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            ["neodoc" :as neodoc]))

(def usage "
spec-extract: Extract EBNF grammars from WebAssembly RST spec files.

Usage:
  spec-extract [options] <files>...

General Options:
  -v, --verbose                     Show verbose output
                                    [env: VERBOSE]
  --debug                           Show debug output
  --omit-whitespace                 Do add explicit whitespace matching to EBNF
  --comments                        Add location comments to EBNF
  --meta-comments                   Add metadata/right side comments to EBNF
")

;;
;; Argument processing
;;
(defn clean-opts [arg-map]
  (reduce (fn [o [a v]]
            (let [k (keyword (S/replace a #"^[-<]*([^>]*)[>]*$" "$1"))]
              (assoc o k (or (get o k) v))))
          {} arg-map))

(defn parse-opts [usage argv & [opts]]
  (-> usage
      (neodoc/run (clj->js (merge {:optionsFirst true
                                   :smartOptions true
                                   :argv (or argv [])}
                                  opts)))
      js->clj
      clean-opts))

;;
;; general utilities
;;
(def Eprn     #(binding [*print-fn* *print-err-fn*] (apply prn %&)))
(def Eprintln #(binding [*print-fn* *print-err-fn*] (apply println %&)))
(def Epprint  #(binding [*print-fn* *print-err-fn*] (pprint %)))
(defn fatal [& msg] (apply Eprintln msg) (js/process.exit 1))


(defn split-match-n
  "Takes a predicate and a collection. The pred function takes
  a collection and returns the number of items matching at head of
  collection (or true/false where true = 1, false/0 = no match). If
  there is no match then all of collection will be in before and match
  and after will be nil.
  Returns:
  [before match after]"
  [pred coll]
  (loop [xs coll, before []]
    (if (empty? xs)
      [(seq before) nil nil]
      (let [n (pred xs)]
        (if (or (nil? n) (false? n) (zero? n))
          (recur (rest xs) (conj before (first xs)))
          (let [n (if (number? n) n 1)]
            [(seq before) (seq (take n xs)) (seq (drop n xs))]))))))

(defn find-match
  "Like split-match-n but returns true or false instead of a count"
  [pred coll]
  (boolean (second (split-match-n pred coll))))


;;
;; parser
;;

(defn strs=
  "Match a sequence of string nodes to strings in strs"
  [nodes & strs]
  (let [nodes (take (count strs) nodes)]
    (and (every? #(= "string" %) (map first nodes))
         (= strs (map second nodes)))))

;; Some predicates for split-match-n
(defn production-name? [[[t c] & xs]]
  (and (= t "macro") (= c "production") 2))

(defn phantom? [[[t1 c1 :as n1] [t2 c2 :as n2] & xs]]
  (and (= n1 ["macro" "phantom"]) (= t2 "group") 2))

(defn environment? [[[t1 c1 :as n1] & xs]]
  (and (= t1 "environment") 1))

(defn production-symbol? [[[t1 c1] & xs]]
  (and (= t1 "macro") (re-seq #"^T" c1)))

(defn equiv? [[[t c] & xs]]
  (and (= t "macro") (= c "equiv")))

(defn production-op? [xs]
  (cond
    (strs= xs "&" ":" ":" "=" "&") 5
    (strs= xs "&" ":" ":" "=")     4
    (strs= xs ":" ":" "=" "&")     4
    (strs= xs ":" ":" "=")         3
    :else nil))

;; "&&\|&", "\|&"
(defn production-alt? [xs]
  (cond
    (strs= xs "&" "&" "|" "&")     4
    (strs= xs "|" "&")             2
    :else nil))


(defn right-arrow? [[n1 n2 & xs]]
  (cond
    (= n1 ["macro" "Rightarrow"])                1
    (= [n1 n2] [["string" "("] ["macro" "iff"]]) 2
    :else 0))

;; rule and production parsing
(defn parse-rule
  "Parse one rule (and metadata if it has it).
  Returns:
  [{:rule rule :metadata metadata} remaining-nodes]"
  [nodes]
  (let [[rule-full alt post] (split-match-n production-alt? nodes)
        [rule _ right] (split-match-n right-arrow? rule-full)]
    [{:rule rule
      :metadata right} post]))

(defn parse-rules
  "Parse the rules of a productions.
  Returns:
  [{:rule rule :metadata metadata} ...]"
  [nodes]
  (loop [rules []
         nodes nodes]
    (if (empty? nodes)
      rules
      (let [[rule nodes] (parse-rule nodes)]
        (recur (conj rules rule) nodes)))))


(defn parse-production
  "Parse one grammar production symbol and rules.
  Returns:
  {:symbol production-symbol :rules rules}"
  [nodes]
  (try
    (let [[_ [psym] nodes] (split-match-n production-symbol? nodes)
          [_ _ nodes] (split-match-n production-op? nodes)
          rules (parse-rules nodes)]
      {:symbol psym
       :rules rules})
    (catch :default e
      {:error (.-message e)})))

(defn split-productions
  "Split sequence of nodes into all its productions.
  Returns:
  [[production-name unparse-nodes] ...]"
  [nodes]
  (loop [productions []
         nodes nodes]
    (if (empty? nodes)
      productions
      (let [[_      [m1 p1] nodes1] (split-match-n production-name? nodes)
            [pnodes [m2 p2] nodes2] (split-match-n production-name? nodes1)]
        (recur (conj productions [p1 pnodes])
               (if p2 (concat [m2 p2] nodes2) []))))))

;;
;; emitter
;;

(defn flatten-nodes
  "Return a sequence of simple nodes with group nodes
  hoisted/flattened"
  [nodes]
  (loop [result [], nodes nodes]
    (if (empty? nodes)
      result
      (let [[[t1 c1 :as n1] & nodes] nodes]
        (if (= "group" t1)
          (recur result (concat c1 nodes))
          (recur (conj result n1) nodes))))))

(defn stringify-nodes
  "Return a plain string representation of a sequence of nodes.
  Unknown node types become 'UNKNOWN_TYPE<type>'. Unknown macros
  become 'UNKNOWN_MACRO<content>' unless raw? is set, in which case
  use plain content for unrecognized macro values."
  [nodes raw?]
  (let [xs (flatten-nodes nodes)
        ss (for [[t1 c1 :as x] xs]
             (condp = t1
               "string"     (condp = c1
                              "~"      " "
                              c1)
               "whitespace" " "
               "macro"      (if (re-seq #"^T[a-z][a-z]*$" c1)
                              (subs c1 1)
                              (condp = c1
                                "text"       ""
                                "ast"        "*"
                                "\\"         " "
                                "backslash" "\\"
                                "$"         "$"
                                "dots"      ""
                                "_"          "_" ;; DANGER: not universal
                                (if raw? c1 (str "UNKNOWN_MACRO<" c1 ">"))))
               (str "UNKNOWN_TYPE<" t1 ">")))]
    (S/join "" ss)))


(defn emit-nodes
  "Take a sequence of parsed/pruned production rule nodes and emit the
  EBNF string equivalent"
  [orig-nodes {:keys [omit-whitespace] :as opts}]
  (let [ws+ (if omit-whitespace " " " ' '+ ")
        ws* (if omit-whitespace " " " ' '* ")]
    (loop [result ""
           nodes orig-nodes]
      (if (empty? nodes)
        result
        (let [[[t1 c1 :as n1] [t2 c2 :as n2] [t3 c3 :as n3] [t4 c4 :as n4]
               [t5 c5 :as n5] [t6 c6 :as n6] [t7 c7 :as n7] [t8 c8 :as n8]] nodes]
          (cond
            ;; Text literals
            ;; ["macro" "text"] ["group" [["string" "br"] ["macro" "_"] ["string" "if"]]]
            ;; ["macro" "text"] ["group" [["string" "offset"] ["group" [["string" "="]]]]]
            (and (= ["macro" "text"] n1)
                 (= "group" t2))
            (recur (str result "'" (stringify-nodes c2) "'") (drop 2 nodes))


            ;; ignore prefixes '.*:' (counts from instructions.rst)
            ;; ["string" "("] ["string" "et"] ["string" ","] ["whitespace" nil] ["string" "y^"] ["macro" "ast"] ["string" ")"] ["group" [["string" ":"]]]
            (= [["string" "("] ["string" "et"] ["string" ","] ["whitespace" nil] ["string" "y^"] ["macro" "ast"] ["string" ")"] ["group" [["string" ":"]]]]
               [n1 n2 n3 n4 n5 n6 n7 n8])
            (recur result (drop 8 nodes))

            ;; 3 ["string" "y"] ["string" ","] ["string" "I"] ["string" "'"] ["group" [["string" ":"]]]
            ;;   ["string" "x"] ["string" ","] ["string" "I"] ["string" "'"] ["group" [["string" ":"]]]
            (and (= "string" t1) (re-seq #"^[a-zA-Z]$" c1)
                 (= [["string" ","] ["string" "I"] ["string" "'"] ["group" [["string" ":"]]]]
                    [n2 n3 n4 n5]))
            (recur result (drop 5 nodes))

            ;;    ["macro" "X"] ["group" [["string" "in"]]] ["string" "_1"] ["group" [["string" ":"]]]
            ;;    ["macro" "X"] ["group" [["string" "in"]]] ["string" "_2"] ["group" [["string" ":"]]]
            (and (= ["macro" "X"] n1)
                 (= "group" t2)
                 (= "string" t3) (re-seq #"^_[IN0-9]$" c3)
                 (= ["group" [["string" ":"]]] n4))
            (recur result (drop 4 nodes))

            ;; 12 ["macro" "X"] ["group" [["string" "in"]]] ["group" [["string" ":"]]]
            ;; 4  ["macro" "X"] ["group" [["string" "bt"]]] ["group" [["string" ":"]]]
            (and (= ["macro" "X"] n1)
                 (= "group" t2)
                 (= ["group" [["string" ":"]]] n3))
            (recur result (drop 3 nodes))

            ;;   ["string" "y^"] ["macro" "ast"] ["group" [["string" ":"]]]
            ;; 4 ["string" "l^"] ["macro" "ast"] ["group" [["string" ":"]]]
            (and (= "string" t1) (re-seq #"^[a-z]\^$" c1)
                 (= ["macro" "ast"] n2)
                 (= ["group" [["string" ":"]]] n3))
            (recur result (drop 3 nodes))

            ;; 4 ["string" "I"] ["string" "'"] ["group" [["string" ":"]]]
            (= [["string" "I"] ["string" "'"] ["group" [["string" ":"]]]] [n1 n2 n3])
            (recur result (drop 3 nodes))

            ;; 333 ["string" "."] ["group" [["string" ":"]]]
            ;; 93  ["string" "laneidx"] ["group" [["string" ":"]]]
            ;; 2   ["string" "l_N"] ["group" [["string" ":"]]]
            (and (= "string" t1) (re-seq #"^[a-zA-Z]" c1)
                 (= ["group" [["string" ":"]]] n2))
            (recur result (drop 2 nodes))

            ;; ["group" [["macro" "pm"]]] ["group" [["string" ":"]]]
            (= [["group" [["macro" "pm"]]] ["group" [["string" ":"]]]] [n1 n2])
            (recur result (drop 2 nodes))


            ;; ignore index/subscript suffixes "_.*"
            ;; ["string" "_"] ["group" [["string" "I"] ["string" "'"]]]
            ;; ["string" "_"] ["group" [["string" "I"] ["string" "'"] ["string" "'"]]]
            (and (= ["string" "_"] n1)
                 (= "group" t2) (= ["string" "I"] (first c2))
                 (every? #(= ["string" "'"] %) (rest c2)))
            (recur result (drop 2 nodes))

            ;; ["string" "_I"]
            (and (= "string" t1) (re-seq #"^_[IN0-9]$" c1))
            (recur result (drop 1 nodes))

            ;; ["string" "_"] ["group" [["string" "16"]]]
            (and (= ["string" "_"] n1)
                 (= "group" t2)
                 (= 1 (count c2))
                 (= "string" (-> c2 first first))
                 (re-seq #"^[0-9][0-9]*$" (-> c2 first second)))
            (recur result (drop 2 nodes))


            ;; repeats/vec zero or more - a*
            (and (= "string" t1) (re-seq #"^t_[0-9][0-9]*" c1)
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

            ;; ["string" ")"] ["string" "^"] ["macro" "ast"]
            (and (= ["string" "^"] n1)
                 (= ["macro" "ast"] n2))
            (recur (str result "* ") (drop 2 nodes))


            ;; repeats/vec specific count - a{N}
            ;; ["string" "^"] ["group" [["string" "16"]]]
            (and (= ["string" "^"] n1)
                 (= "group" t2)
                 (re-seq #"^[0-9][0-9]*$" (-> c2 first second)))
            (recur (str result " ){" (-> c2 first second) "} ") (drop 2 nodes))

            ;; repeats one or more - a+
            ;; ["string" "^"] ["string" "+"]
            (and (= ["string" "^"] n1)
                 (= ["string" "+"] n2))
            (recur (str result "+ ") (drop 2 nodes))

            ;; optional - a?
            ;; ["macro" "Tid"] ["string" "_1^"] ["string" "?"]
            ;; ["macro" "Tid"] ["string" "_2^"] ["string" "?"]
            (and (= "macro" t1) (re-seq #"^T" c1)
                 (or (= ["string" "^"] n2)
                     (and (= "string" t2) (re-seq #"^_[0-9][0-9]*\^$" c2)))
                 (= ["string" "?"] n3))
            (recur (str result "(" (subs c1 1) ")?") (drop 3 nodes))

            ;; ["string" "^"] ["string" "?"]
            (and (= ["string" "^"] n1)
                 (= ["string" "?"] n2))
            (recur (str result "?") (drop 2 nodes))


            ;; other literals
            (= ["string" "("] n1)
            (recur (str result "( ") (drop 1 nodes))

            (= ["string" ")"] n1)
            (recur (str result " )") (drop 1 nodes))


            ;; "~~|~~", "~|~"
            (strs= [n1 n2 n3 n4 n5] "~" "~" "|" "~" "~")
            (recur (str result " | ") (drop 5 nodes))

            (strs= [n1 n2 n3] "~" "|" "~")
            (recur (str result " | ") (drop 3 nodes))

            (= ["macro" "epsilon"] n1)
            (recur (str result "'' ") (drop 1 nodes))

            ;; ["macro" "unicode"] ["group" [["string" "00"]]]
            (and (= ["macro" "unicode"] n1)
                 (= "group" t2) (= "string" (-> c2 first first)))
            (recur (str result "\"\\u" (-> c2 first second (.padStart 4 "0")) "\"")
                   (drop 2 nodes))

            ;; ["macro" "T"] ["group" [["string" "eof"]]]
            (= [["macro" "T"] ["group" [["string" "eof"]]]] [n1 n2])
            (recur (str result "EOF") (drop 2 nodes))


            ;; terminals
            (and (= "macro" t1) (re-seq #"T[uif]" c1)
                 (= "string" t2) (re-seq #"^[0-9][0-9]*$" c2))
            (recur (str result (subs c1 1) c2) (drop 2 nodes))

            (and (= "macro" t1) (re-seq #"^T" c1))
            (recur (str result (subs c1 1)) (drop 1 nodes))


            ;; whitespace
            ;; ["macro" "\\"] ["whitespace" nil] ["string" "|"] ["whitespace" nil] ["macro" "\\"]
            (= [["macro" "\\"] ["whitespace" nil] ["string" "|"] ["whitespace" nil] ["macro" "\\"]] [n1 n2 n3 n4 n5])
            (recur (str result " ") (drop 5 nodes))

            (= ["macro" "\\"] n1)
            (recur (str result " ") (drop 1 nodes))

            (= "whitespace" t1)
            (recur (str result " ") (drop 1 nodes))

            (and (= ["string" "~"] n1)
                 (= ["string" "~"] n2))
            (recur (str result ws+) (drop 2 nodes))

            (= ["string" "~"] n1)
            (recur (str result ws*) (drop 1 nodes))

            (= ["macro" "quad"] n1)
            (recur (str result " ") (drop 1 nodes))

            (= ["macro" "qquad"] n1)
            (recur (str result "  ") (drop 1 nodes))

            (= ["string" "&"] n1)
            (recur (str result " ") (drop 1 nodes))


            ;; misc
            (= ["macro" "dots"] n1)
            (recur (str result " ") (drop 1 nodes))

            ;; ["string" "["] ["string" "-"] ["string" "2ex"] ["string" "]"]
            (and (= ["string" "["] n1)
                 (= ["string" "-"] n2)
                 (= ["string" "2ex"] n3)
                 (= ["string" "]"] n4))
            (recur result (drop 4 nodes))


            :else
            (let [unmatched (take 10 nodes)
                  cont (if (> (count nodes) 10) "..." "")]
              (throw (js/Error. (str "unmatched nodes: \n" unmatched cont
                                     "\nemit start: \n" (take 30 orig-nodes)))))))))))

(defn emit-production
  "Emit EBNF string for one production"
  [{:keys [symbol files rules]} opts]
  (let [plen (count symbol)
        rule-delim (str "\n" (apply str (repeat plen " ")) " |   ")]
    (str symbol " ::= "
         (S/join rule-delim
                 (for [{:keys [rule metadata file]} rules
                       :let [ebnf (emit-nodes rule opts)]
                       :when (not (re-seq #"^ *$" (stringify-nodes rule)))
                       :let [ebnf (if (and (:comments opts)
                                           (> (count files) 1))
                                    (str ebnf " (* file: " file " *)")
                                    ebnf)
                             ebnf (if (:meta-comments opts)
                                    (str ebnf " (* => "
                                         (stringify-nodes metadata true)
                                         " *)")
                                    ebnf)]]
                   ebnf)))))

(defn parse-latex
  "Parse latex into abstract syntax tree (AST)"
  [parser math-text]
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

(defn prune-latex
  "Does the following:
  - Remove AST header
  - Convert AST maps into [type content] nodes (removes position)
  - Remove \\phantom wrapper tags (hoist content)
  - Remove 'environment' tags (hoist content)"
  [ast]
  (let [;; Flatten and only keep type/content (drop position)
        ast (walk/postwalk
              #(if (map? %) [(:type %) (:content %)] %)
              ast)
        ;; hoist content of phantom tags (leaving just the content)
        ast (loop [result [], ast ast]
              (let [[before [_ [_ pgroup]] after] (split-match-n phantom? ast)
                    pgroup (if (and (= 1 (count pgroup))
                                    (= "string" (ffirst pgroup)))
                             []
                             pgroup)
                    result (concat result before pgroup)]
                (if after
                  (recur result after)
                  result)))
        ;; hoist content of environment tags (leaving just the content)
        ast (loop [result [], ast ast]
              (let [[before [[_ pgroup]] after] (split-match-n environment? ast)
                    result (concat result before pgroup)]
                (if after
                  (recur result after)
                  result)))]
    ast))

;;
;; File and section handling
;;

(defn clean-math-text
  "Remove whitespace/indentation"
  [text]
  (-> text
      (S/replace #"^\s+" "")  ; Remove leading whitespace
      (S/replace #"\n\s+" "\n")  ; Normalize indentation
      (S/trim)))

(defn extract-math-blocks
  "Parse and return latex math sections from an RST document"
  [content]
  (let [math-pattern #"(?s)\.\. math::\s*\n\s*(.+?)(?=\n\n|$)"]
    (->> (re-seq math-pattern content)
         (map second)
         (map clean-math-text))))

(defn parse-files
  "Load RST spec files, parse latex from math sections, split the
  production nodes into rules."
  [files {:as opts :keys [verbose debug]}]
  (let [ast-parser (-> (unified.)
                       (.use unifiedLatexFromStringMinimal #js {:mode "math"}))]
    (for [file files
          :let [_ (when (or verbose debug)
                    (Eprintln "Processing:" file))
                content (fs/readFileSync file "utf-8")]
          block (extract-math-blocks content)
          :let [_ (when debug
                    (Eprintln "== Raw Math Block ==")
                    (Eprintln block))]
          :let [full-ast (parse-latex ast-parser block)
                ast (prune-latex (get-in full-ast [:ast :content 0 :content]))]
          :when (and (find-match production-name? ast)
                     (not (find-match equiv? ast)))
          [name-node ast] (split-productions ast)
          :let [{:keys [symbol rules]} (parse-production ast)
                rules (for [rule rules] (assoc rule :file file))]]
      {:files #{file}
       :block block
       :ast ast
       :name (stringify-nodes [name-node] true)
       :symbol (stringify-nodes [symbol] true)
       :rules rules})))

(defn merge-production-rules
  "Combine production rules that are defined incrementally in the same
  file or across multiple files.
  Returns (a map):
  {symbol production, ...}"
  [productions]
  (reduce (fn [ps {:as p :keys [files symbol rules]}]
            (if (get-in ps [symbol])
              (-> ps
                  (update-in [symbol :rules] concat rules)
                  (update-in [symbol :files] into files))
              (assoc-in ps [symbol] p)))
          {} productions))

(def PRODUCTION-OVERRIDES
  {"char"     "char ::= #\"^[^\\x00-\\x1F\\x7F-\\x9F]$\""
   "digit"    "digit ::= #\"^[0-9]$\""
   "hexdigit" "hexdigit ::= digit\n          | #\"^[A-Fa-f]$\""
   "keyword"  "keyword ::= ( #\"^[a-z]$\" ) idchar*"
   "idchar"   "idchar ::= #\"^[0-9A-Za-z!#$%&'*+\\-./:<=>?@\\\\^_`|~]$\""})

(defn add-production-ebnfs
  [productions opts]
  (into {} (for [[symbol prod] productions]
             (let [prod (if-let [e (get PRODUCTION-OVERRIDES symbol)]
                          (merge prod {:ebnf e
                                       :files #{"<OVERRIDE>"}})
                          (merge prod {:ebnf (emit-production prod opts)}))]
               [symbol prod]))))



(defn -main
  "Load the spec RST files, parse them, and emit EBNF grammars."
  [& args]
  (let [{:as opts :keys [files]} (parse-opts usage args)
        _ (when (empty? files)
            (fatal "At least one file required"))
        _ (when (some #{"-h" "--help"} files)
            (fatal usage))
        raw-productions (parse-files files opts)
        file-sym-productions (merge-production-rules raw-productions)
        ebnf-productions (add-production-ebnfs file-sym-productions opts)]
    ;; TODO: merge repeated production names
    (Eprintln "\nEBNF Productions:")
    (doseq [[symbol {:keys [files error name ebnf]}] ebnf-productions]
      (if (not error)
        (do
          (when (:comments opts)
            (println (str "(* " name " ("
                          "files: '" (S/join "', '" files) "'"
                          ") *)")))
          (println ebnf))
        (do
          (Eprintln "\n\n===== Error =====")
          (Eprintln "== Location ==")
          (Eprintln (str "file: " (S/join "," files) ", production name: " name))
          (Eprintln "== Error Message ==")
          (Eprintln (.-message error))
          (throw error))))))

(apply -main *command-line-args*)

