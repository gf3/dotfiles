;;; init-apheleia.el --- Formatter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package apheleia
  :straight (:host github :repo "radian-software/apheleia" :branch "main")
  :defer t
  :hook ((prog-mode . apheleia-mode))
  :commands (apheleia-mode apheleia-global-mode apheleia-format-buffer)
  :config
  ;; Additional formatters
  (push '(jsbeautify-css . (npx "js-beautify" "--type=css" "--quiet" "-"))
		apheleia-formatters)
  (push '(jsbeautify-html . (npx "js-beautify" "--type=html" "--quiet" "-"))
		apheleia-formatters)
  (push '(jsbeautify-js . (npx "js-beautify" "--type=js" "--quiet" "-"))
		apheleia-formatters))

(provide 'init-apheleia)
;;; init-apheleia.el ends here
