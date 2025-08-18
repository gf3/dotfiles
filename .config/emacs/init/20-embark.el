;;; 20-embark.el --- Minibuffer actions. -*- lexical-binding: t -*-

;; Author: Gianni Chiappetta
;; Maintainer: Gianni Chiappetta
;; Version: version
;; Package-Requires: (marginalia embark embark-consult)
;; Homepage: homepage
;; Keywords: keywords

;;; Commentary:

;; None.

;;; Code:

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide '20-embark)
;;; 20-embark.el ends here
