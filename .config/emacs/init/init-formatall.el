;;; init-formatall.el --- Formatter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package format-all
  :straight t
  :hook ((prog-mode . format-all-mode)
		 (prog-mode . format-all-ensure-formatter))
  :config
  (define-format-all-formatter js-beautify
							   (:executable "js-beautify")
							   (:install "npm -g install js-beautify")
							   (:languages "CSS" "HTML" "JavaScript" "JSON" "JSON5" "JSX" "SCSS" "TSX" "TypeScript")
							   (:features)
							   (:format (format-all--buffer-easy executable (or (buffer-file-name) (buffer-name)))))
  (add-hook 'json-mode-hook #'(lambda ()
                                (setq-local format-all-formatters '(("JSON" js-beautify)
																	("JSON5" js-beautify)))))
  (add-hook 'web-mode-hook #'(lambda ()
                               (setq-local format-all-formatters '(("HTML" js-beautify)
																   ("CSS" js-beautify)
																   ("Less" js-beautify)
																   ("SCSS" js-beautify))))))

(provide 'init-formatall)
;;; init-formatall.el ends here
