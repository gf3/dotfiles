;;; 20-popwin.el --- Ugh windows. -*- lexical-binding: t -*-

;; Author: Gianni Chiappetta
;; Maintainer: Gianni Chiappetta
;; Version: 0
;; Package-Requires: (popwin)
;; Homepage: homepage
;; Keywords: keywords

;;; Commentary:

;; commentary

;;; Code:

(use-package popwin
  :straight t
  :map (("C-z" . popwin:keymap))
  :init
  (push "*Shell Command Output*" popwin:special-display-config)
  (push "*Async Shell Command*" popwin:special-display-config)
  (push "*vc-diff*" popwin:special-display-config)
  (push "*vc-change-log*" popwin:special-display-config)
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (popwin-mode 1))

(provide '20-popwin)

;;; 20-popwin.el ends here
