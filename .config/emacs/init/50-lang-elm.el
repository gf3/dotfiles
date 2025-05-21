;;; 50-lang-elm.el --- Elm configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :straight t
  :after eglot
  :mode (("\\.elm\\'" . elm-mode))
  :hook ((elm-mode . elm-format-on-save-mode))
  :config
  (global-subword-mode t)
  (add-to-list 'eglot-server-programs '(elm-mode "elm-language-server")))

(provide '50-lang-elm)
;;; 50-lang-elm.el ends here
