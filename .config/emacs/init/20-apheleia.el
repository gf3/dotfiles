;;; 20-apheleia.el --- Formatter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package apheleia
  :straight t
  :defer t
  :hook ((prog-mode . apheleia-mode))
  :commands (apheleia-mode apheleia-global-mode apheleia-format-buffer)
  :config
  ;; Additional formatters
  (push '(mix-filename-format . ("mix" "format" "--stdin-filename" filepath "-"))
		    apheleia-formatters)
  (push '(jsbeautify-css . (npx "js-beautify" "--type=css" "--quiet" "-"))
		    apheleia-formatters)
  (push '(jsbeautify-html . (npx "js-beautify" "--type=html" "--quiet" "-"))
		    apheleia-formatters)
  (push '(jsbeautify-js . (npx "js-beautify" "--type=js" "--quiet" "-"))
		    apheleia-formatters)
  (push '(erb-formatter . ("erb-format" "--stdin"))
		    apheleia-formatters)
  (push '(rufo . ("rufo" "--simple-exit"))
		    apheleia-formatters)
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
		    '(rufo)))

(provide '20-apheleia)
;;; 20-apheleia.el ends here
