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

(defn find-match-n
  [pred coll]
  (boolean (second (split-match-n pred coll))))


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

;; "&&\|&", "\|&", "~~|~~", "~|~"
(defn production-alt? [xs]
  (cond
    (strs= xs "~" "~" "|" "~" "~") 5
    (strs= xs "&" "&" "|" "&")     4
    (strs= xs "~" "|" "~")         3
    (strs= xs "|" "&")             2
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

(defn split-productions [ast]
  (loop [productions []
         nodes ast]
    (if (empty? nodes)
      productions
      (let [[_      [m1 p1] nodes1] (split-match-n production-name? nodes)
            [pnodes [m2 p2] nodes2] (split-match-n production-name? nodes1)]
        (recur (conj productions [p1 pnodes])
               (if p2 (concat [m2 p2] nodes2) []))))))

;;
;; emitter
;;

(defn flatten-groups [x]
  (if (= (first x) "group")
    (mapcat flatten-groups (second x))
    [(second x)]))

(defn flatten-nodes [xs]
  (loop [result [], xs xs]
    (if (empty? xs)
      result
      (let [[[t1 c1 :as x] & xs] xs]
        (if (= "group" t1)
          (recur result (concat c1 xs))
          (recur (conj result x) xs))))))

(defn stringify-nodes [nodes raw?]
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
                                "_"          "_" ;; DANGER: not universal
                                (if raw? c1 (str "UNKNOWN_MACRO<" c1 ">"))))
               (str "UNKNOWN_TYPE<" t1 ">")))]
    (S/join "" ss)))


(defn emit-nodes [orig-nodes {:keys [auto-whitespace? raw?] :as opts}]
  (let [ws (if auto-whitespace? " " " ' '* ")]
    (loop [result ""
           nodes orig-nodes]
      (if (empty? nodes)
        result
        (let [[[t1 c1 :as n1] [t2 c2 :as n2] [t3 c3 :as n3] [t4 c4 :as n4]
               [t5 c5 :as n5] [t6 c6 :as n6] [t7 c7 :as n7] [t8 c8 :as n8]] nodes
              f2 (flatten-nodes (take 2 nodes))
              s2 (stringify-nodes (take 2 nodes))]
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
            (= ["string" "y^"] ["macro" "ast"] ["group" [["string" ":"]]] n1)
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
            (recur (str result ") ") (drop 1 nodes))

            (= ["macro" "epsilon"] n1)
            (recur (str result "'' ") (drop 1 nodes))


            ;; Handle raw?
            (and raw? (= "group" t1))
            (recur (str result (emit-nodes c1 opts)) (drop 1 nodes))

            (and raw? (= "string" t1))
            (recur (str result c1) (drop 1 nodes))


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
            (recur (str result (if raw? " " ws)) (drop 1 nodes))

            (and (= ["string" "~"] n1)
                 (= ["string" "~"] n2))
            (recur (str result ws) (drop 2 nodes))

            (= ["string" "~"] n1)
            (recur (str result ws) (drop 1 nodes))

            (= ["macro" "quad"] n1)
            (recur (str result " ") (drop 1 nodes))

            (= ["macro" "qquad"] n1)
            (recur (str result "  ") (drop 1 nodes))

            (= ["string" "&"] n1)
            (recur (str result " ") (drop 1 nodes))


            ;; drop junk
            (= ["macro" "dots"] n1)
            (recur (str result " <DOTS> ") (drop 1 nodes))

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
                                     "\nnode start: \n" (take 10 orig-nodes)))))))))))

(defn emit-production [symbol rules opts]
  (let [pname (subs (second symbol) 1) ;; TODO:
        plen (count pname)
        rule-delim (str "\n" (apply str (repeat plen " ")) " |   ")]
    (str pname " ::= "
         (S/join rule-delim
                 (for [{:keys [rule metadata]} rules]
                   (str (emit-nodes rule (assoc opts :meta? false))
                        "(* => "
                        (stringify-nodes metadata true)
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

(defn prune-latex [ast]
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

(defn load-files [files {:as opts :keys [verbose? debug?]}]
  (let [ast-parser (-> (unified.)
                       (.use unifiedLatexFromStringMinimal #js {:mode "math"}))]
    (for [file files
          :let [_ (when (or verbose? debug?)
                    (println "Processing:" file))
                content (fs/readFileSync file "utf-8")]
          block (extract-math-blocks content)
          :let [_ (when debug?
                    (println "== Raw Math Block ==")
                    (println block))]
          :let [full-ast (parse-latex ast-parser block)
                ast (prune-latex (get-in full-ast [:ast :content 0 :content]))]
          :when (and (find-match-n production-name? ast)
                     (not (find-match-n equiv? ast)))
          [name-node ast] (split-productions ast)]
      {:file file
       :block block
       :name (emit-nodes [name-node] (assoc opts :raw? true))
       :ast ast})))


(defn -main [& files]
  (let [bubble-errors? false
        opts {:auto-whitespace? true :verbose? false :debug? false}
        raw-productions (load-files files opts)
        all-productions (reduce
                          (fn [ps {:keys [file block name ast] :as p}]
                            (try
                              (let [{:keys [symbol rules]} (parse-production ast)
                                    sname (emit-nodes [symbol] (assoc opts :meta? true))
                                    ebnf (emit-production symbol rules opts)]
                                (conj ps (merge p {:symbol sname
                                                   :ast ast
                                                   :ebnf ebnf})))
                              (catch :default e
                                (if bubble-errors?
                                  (throw e)
                                  (conj ps (merge p {:error e}))))))
                          [] raw-productions)
        file-productions (group-by :file all-productions)]
    ;; TODO: merge repeated production names
    (println "\nEBNF Productions:")
    (doseq [[file productions] file-productions]
      (println (str "\n(* file: " file " *)\n"))
      (doseq [{:keys [block error name symbol ast ebnf]} productions]
        (if error
          (do
            (println "\n\n===== Error =====")
            (println "== Location ==")
            (println (str "file: " file ", production name: " name))
            (println "== Error Message ==")
            (println (.-message error))
            (throw error))
          (do
            (println (str "(* production '" name "' (symbol: '" symbol "') *)"))
            (println ebnf)))))))

(apply -main *command-line-args*)


