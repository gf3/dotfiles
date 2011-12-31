(def completions (mapcat (comp keys ns-publics) (all-ns)))
 
(with-open [f (java.io.BufferedWriter. (java.io.FileWriter. (str (System/getenv "HOME") "/.clj_completions")))]
  (.write f (apply str (interpose \newline completions))))
