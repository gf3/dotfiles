;-
; Copyright 2009-2011 (c) Meikel Brandmeyer.
; All rights reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

(ns vimclojure.util
  (:require
    [clojure.pprint :as pprint]
    [clojure.stacktrace :as stacktrace]))

; Common helpers
(defn str-cut
  "Cut n characters of the end of the string s."
  [string n]
  (.substring string 0 (- (.length string) n)))

(defn str-wrap
  "Wrap the given string into the given separators."
  ([string sep]
   (str-wrap string sep sep))
  ([string before after]
   (str before string after)))

(defn str-cat
  "Concatenate the given collection to a string, separating the
  collection's items with the given separator."
  [coll sep]
  (apply str (interpose sep coll)))

(defn splitted-match
  "Splits pattern and candidate at the given delimiters and matches
  the parts of the pattern with the parts of the candidate. Match
  means „startsWith“ here."
  [pattern candidate delimiters]
  (if-let [delimiters (seq delimiters)]
    (let [delim           (first delimiters)
          pattern-split   (.split pattern delim)
          candidate-split (.split candidate delim)]
      (and (<= (count pattern-split) (count candidate-split))
           (reduce #(and %1 %2) (map #(splitted-match %1 %2 (rest delimiters))
                                     pattern-split
                                     candidate-split))))
    (.startsWith candidate pattern)))

; Command-line handling
(defn print-usage
  "Print usage information for the given option spec."
  [description specs]
  (println description)
  (newline)
  (println "Options:")
  (doseq [spec (filter vector? specs)]
    (let [loption (name (first spec))
          spec    (rest spec)
          soption (when (symbol? (first spec)) (name (first spec)))
          spec    (if soption (rest spec) spec)
          descr   (first spec)
          default (first (rest spec))]
      (print (format "  --%-10s " loption))
      (when soption
        (print (format "-%-3s " soption)))
      (print descr)
      (when default
        (newline)
        (print (format "                    The default is '%s'." default))))
    (newline))
  (flush))

(defn with-command-line*
  "Parse the command line arguments according to the given specifications.
  A specification consists of a vector of an option name, an optional short
  option name, a description and an optional default value. An option name
  ending in ? designates a boolean flag. The last entry in the list of
  specifications might be a symbol which is bound to the rest of the command
  line arguments when -- or the first non-option argument is encountered.

  -h, --help or -? stop the parsing and trigger the printing of the usage
  message and thunk is not called.

  thunk is called with a map of option-value pairs found on the command line."
  [args description specs thunk]
  (let [[options soptions]
        (reduce (fn [[opts sopts] spec]
                  (let [lopt  (name (first spec))
                        sopt  (second spec)
                        sopt  (if (symbol? sopt) (name sopt) nil)
                        [lopt sopt type]
                        (if (.endsWith lopt "?")
                          [(str-cut lopt 1) sopt :flag]
                          [lopt             sopt :option])]
                    (vector (assoc opts lopt type)
                            (assoc sopts sopt lopt))))
                [{} {}]
                (filter vector? specs))
        rest-arg (when (symbol? (last specs)) (name (last specs)))]
    (loop [args   (seq args)
           argmap (hash-map)]
      (let [arg (first args)]
        (cond
          (empty? args)  (if-not rest-arg
                           (thunk argmap)
                           (throw
                             (Exception. "Missing command line arguments")))

          (some #{arg} ["-h" "--help" "-?"])
          (print-usage description specs)

          (= arg "--")
          (if rest-arg
            (thunk (assoc argmap rest-arg (rest args)))
            (throw (Exception.
                     "Unexpected command line arguments")))

          (.startsWith arg "--")
          (let [option (.substring arg 2)]
            (condp = (options option)
              :flag   (recur (rest args) (assoc argmap option true))
              :option (if-let [value (second args)]
                        (recur (nthnext args 2) (assoc argmap option value))
                        (throw (Exception.
                                 (str "Missing value for option: " arg))))
              nil     (throw (Exception. (str "Unknown option: " option)))))

          (.startsWith arg "-")
          (let [option (.substring arg 1)]
            (if-let [loption (soptions option)]
              (recur (cons (str "--" loption) (rest args)) argmap)
              (throw (Exception. (str "Unknown option: " option)))))

          :else
          (if rest-arg
            (thunk (assoc argmap rest-arg args))
            (throw (Exception.
                     "Unexpected command line arguments"))))))))

(defmacro with-command-line
  "Parses the command line arguments given according to the specifications.
  A specification consists of a vector of an option name, an optional short
  option name, a description and an optional default value. An option name
  ending in ? designates a boolean flag. The last entry in the list of
  specifications might be a symbol which is bound to the rest of the command
  line arguments when -- or the first non-option argument is encountered.

  -h, --help or -? stop the parsing and trigger the printing of the usage
  message and body is not executed.

  The body is executed with the long option names bound to the value found
  on the command line or the default value if the option was not given.
  Flags default to nil, ie. logical false."
  [args description specs & body]
  (let [defaults (map (fn [spec]
                        (cond
                          (not (vector? spec)) [spec nil]

                          (-> spec first name (.endsWith "?"))
                          (vector (-> spec first name (str-cut 1) symbol) false)

                          (-> spec second symbol?)
                          (vector (first spec) (when (= (count spec) 4)
                                                 (nth spec 3)))

                          :else
                          (vector (first spec) (when (= (count spec) 3)
                                                         (nth spec 2)))))
                      specs)]
    `(with-command-line* ~args
       ~description
       (quote ~specs)
       (fn [{:strs ~(vec (map first defaults))
             :or   ~(into {} defaults)}]
         ~@body))))

; Vim Interface:
(defmulti
  #^{:arglists '([thing])
     :doc
  "Convert the Clojure thing into a Vim thing."}
  clj->vim
  class)

(defmethod clj->vim :default
  [thing]
  (str-wrap thing \"))

(defmethod clj->vim Boolean
  [thing]
  (if thing "1" "0"))

(derive clojure.lang.ISeq              ::ToVimList)
(derive clojure.lang.IPersistentSet    ::ToVimList)
(derive clojure.lang.IPersistentVector ::ToVimList)

(defmethod clj->vim ::ToVimList
  [thing]
  (str-wrap (str-cat (map clj->vim thing) ", ") \[ \]))

(derive clojure.lang.IPersistentMap ::ToVimDict)

(defmethod clj->vim ::ToVimDict
  [thing]
  (str-wrap (str-cat (map (fn [[kei value]]
                            (str (clj->vim kei) " : " (clj->vim value)))
                          thing)
                     ", ")
            \{ \}))

(defmethod clj->vim String
  [thing]
  (pr-str thing))

(defmethod clj->vim clojure.lang.Named
  [thing]
  (if-let [prefix (namespace thing)]
    (str-wrap (str prefix "/" (name thing)) \")
    (str-wrap (name thing) \")))

(defmethod clj->vim Number
  [thing]
  (str thing))

(defmethod clj->vim nil
  [_]
  0)

(defn safe-var-get
  [the-var]
  (when (.isBound the-var)
    (var-get the-var)))

(defn decide-completion-in
  [nspace prefix base]
  (let [nom (name prefix)]
    (if (pos? (count nom))
      (cond
        (or (contains? (set (map ns-name (all-ns))) prefix)
            (contains? (ns-aliases nspace) prefix))
        [:local-var]

        (or (Character/isUpperCase (char (first nom)))
            (try
              (instance? Class (ns-resolve nspace prefix))
              (catch ClassNotFoundException _ false)))
        [:static-field]

        :else (throw (Exception. "Cannot determine type of prefix")))
      (cond
        (Character/isUpperCase (char (first base))) [:import]
        (< -1 (.indexOf base (int \.)))             [:namespace]
        :else [:full-var :alias :namespace]))))

(defn- type-of-completion
  [thing]
  (cond
    (instance? clojure.lang.Namespace thing)   "n"
    (instance? java.lang.reflect.Field thing)  "S"
    (instance? java.lang.reflect.Method thing) "M"
    (class? thing)        "c"
    (coll? thing)         (recur (first thing))
    (:macro (meta thing)) "m"
    :else                 (let [value (safe-var-get thing)]
                            (cond
                              (instance? clojure.lang.MultiFn value) "f"
                              (fn? value) "f"
                              :else       "v"))))

(defmulti make-completion-item
  "Create a completion item for Vim's popup-menu."
  (fn [_ the-thing] (type-of-completion the-thing)))

(defmethod make-completion-item "n"
  [the-name the-space]
  (let [docs (-> the-space meta :doc)
        info (str " " the-name \newline
                  (when docs (str \newline docs)))]
    (hash-map "word" the-name
              "kind" "n"
              "menu" ""
              "info" info)))

(defmethod make-completion-item "c"
  [the-name _]
  (hash-map "word" the-name
            "kind" "c"
            "menu" ""
            "info" ""))

(defmethod make-completion-item "M"
  [the-name the-methods]
  (let [nam      (name (read-string the-name))
        rtypes   (map #(-> % .getReturnType .getSimpleName) the-methods)
        arglists (map (fn [m]
                        (let [types (.getParameterTypes m)]
                          (vec (map #(.getSimpleName %) types))))
                      the-methods)
        info     (apply str "  " the-name \newline \newline
                        (map #(str "  " %1 " " nam
                                   (str-wrap (str-cat %2 ", ") \( \))
                                   \; \newline)
                             rtypes arglists))]
    (hash-map "word" the-name
              "kind" "M"
              "menu" (print-str arglists)
              "info" info)))

(defmethod make-completion-item "S"
  [the-name [the-field]]
  (let [nam  (name (read-string the-name))
        menu (-> the-field .getType .getSimpleName)
        info (str "  " the-name \newline \newline
                  "  " menu " " the-name \newline)]
    (hash-map "word" the-name
              "kind" "S"
              "menu" menu
              "info" info)))

(defmethod make-completion-item "v"
  [the-name the-var]
  (let [info (str "  " the-name \newline)
        info (if-let [docstring (-> the-var meta :doc)]
               (str info \newline "  " docstring)
               info)]
    (hash-map "word" the-name
              "kind" "v"
              "menu" (pr-str (try
                               (type @the-var)
                               (catch IllegalStateException _
                                 "<UNBOUND>")))
              "info" info)))

(defn- make-completion-item-fm
  [the-name the-fn typ]
  (let [info     (str "  " the-name \newline)
        metadata (meta the-fn)
        arglists (:arglists metadata)
        info     (if arglists
                   (reduce #(str %1 "  " (prn-str (cons (symbol the-name) %2)))
                           (str info \newline) arglists)
                   info)
        info     (if-let [docstring (:doc metadata)]
                   (str info \newline "  " docstring)
                   info)]
    (hash-map "word" the-name
              "kind" typ
              "menu" (pr-str arglists)
              "info" info)))

(defmethod make-completion-item "f"
  [the-name the-fn]
  (make-completion-item-fm the-name the-fn "f"))

(defmethod make-completion-item "m"
  [the-name the-fn]
  (make-completion-item-fm the-name the-fn "m"))

; Namespace helpers
(defn resolve-and-load-namespace
  "Loads and returns the namespace named by the given string or symbol."
  [namespace]
  ; Special case for user: make sure it always exists for the Repl.
  (binding [*ns* *ns*]
    (in-ns 'user))
  (let [namespace (if (symbol? namespace) namespace (symbol namespace))]
    (try
      (the-ns namespace)
      (catch Exception _
        (require namespace)
        (the-ns namespace)))))

(defn stream->seq
  "Turns a given stream into a seq of Clojure forms read from the stream."
  [stream]
  (let [eof (Object.)
        rdr (fn [] (read stream false eof))]
    (take-while #(not= % eof) (repeatedly rdr))))

; Pretty printing.
(defn pretty-print
  "Print the given form in a pretty way. If Tom Faulhaber's pretty printer is
  not installed simply defaults prn."
  [form]
  (pprint/pprint form))

(defn pretty-print-code
  "Print the given form in a pretty way. If Tom Faulhaber's pretty printer is
  not installed simply defaults prn. Uses the *code-dispatch* formatting."
  [form]
  (pprint/with-pprint-dispatch pprint/code-dispatch
    (pprint/pprint form)))

(defn pretty-print-stacktrace
  "Print the stacktrace of the given Throwable. Tries clj-stacktrace,
  clojure.stacktrace and clojure.contrib.stacktrace in that order. Otherwise
  defaults to simple printing."
  [e]
  (stacktrace/print-stack-trace e))

(defn pretty-print-causetrace
  "Print the causetrace of the given Throwable. Tries clj-stacktrace,
  clojure.stacktrace and clojure.contrib.stacktrace in that order. Otherwise
  defaults to simple printing."
  [e]
  (stacktrace/print-cause-trace e))

; Load optional libraries
(defmacro defoptional
  [sym args & body]
  `(let [docstring# (:doc (meta (var ~sym)))]
     (defn ~sym ~args ~@body)
     (alter-meta! (var ~sym) assoc :doc docstring#)))

(try
  (load "optional/clj_stacktrace")
  (catch Exception exc
    (when-not (re-find #"Could not locate clj_stacktrace/repl__init.class or clj_stacktrace/repl.clj on classpath" (str exc))
      (throw exc))))
