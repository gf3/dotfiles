;;; init-modeline.el --- Mode line configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar hidden-minor-modes
  '(apheleia-mode
    auto-revert-mode
    beacon-mode
    company-box-mode
    company-mode
    copilot-mode
    eldoc-mode
    eldoc-box-hover-at-point-mode
    exunit-mode
    flycheck-mode
    flyspell-mode
    undo-tree-mode
    which-key-mode
    yas-minor-mode))

(defun gf3/purge-minor-modes ()
  "Remove minor modes from the mode line."
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assq x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'gf3/purge-minor-modes)

(use-package mood-line
  :straight t
  :demand t

  :custom
  (mood-line-segment-modal-meow-state-alist
   '((normal "🅽" . font-lock-variable-name-face)
     (insert "🅸" . font-lock-string-face)
     (keypad "🅺" . font-lock-keyword-face)
     (beacon "🅱" . font-lock-type-face)
     (motion "🅼" . font-lock-constant-face)))

  ;; Enable mood-line
  :config
  (mood-line-mode 1)

  ;; Use pretty unicode glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode))

(provide 'init-modeline)
;;; init-modeline.el ends here
