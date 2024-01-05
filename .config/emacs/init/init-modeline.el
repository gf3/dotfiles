;;; init-modeline.el --- Mode line configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package doom-modeline
;;   :straight t
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 32)
;;   (doom-modeline-hud t)
;;   (doom-modeline-support-imenu t)
;;   (doom-modeline-buffer-encoding nil))

;; (when (display-graphic-p)
;;   (require 'all-the-icons))

;; ("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified
;;    mode-line-remote mode-line-window-dedicated)
;;   display (min-width (6.0)))
;;  mode-line-frame-identification mode-line-buffer-identification "   "
;;  mode-line-position (project-mode-line project-mode-line-format)
;;  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info
;;  mode-line-end-spaces)

;; (defun custom-modeline-mode-icon ()
;;   (format " %s"
;;           (propertize icon
;;                       'help-echo (format "Major-mode: `%s`" major-mode)
;;                       'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

;; (setq mode-line-format
;;       (list
;;        ;; Buffer name
;;        '(:eval "%b" 'face 'font-lock-keyword-face 'help-echo (buffer-file-name))
;;        " ("
;;        '(:eval (custom-modeline-mode-icon))
;;        ") "
;;        ;; Read only?
;;        '(:eval (when buffer-read-only
;;                  (propertize (all-the-icons-octicon "lock") 'face 'font-lock-warning-face 'help-echo "Buffer is read-only")))
;;        " "
;;        ;; Major mode
;;        '(:eval (propertize "%m" 'face 'font-lock-string-face 'help-echo buffer-file-coding-system))))

;; (setq mode-line-format '(""
;;                          ;; mode-line-mule-info
;;                          ;; mode-line-modified
;;                          mode-line-frame-identification
;;                          mode-line-buffer-identification
;;                          "  "
;;                          ;; mode-line-position
;;                          (vc-mode vc-mode)
;;                          "  "
;;                          mode-line-modes
;;                          (which-function-mode ("" which-func-format " "))
;;                          (global-mode-string (" " global-mode-string))
;;                          )
;;       ;; "-%-")
;;       )

(defvar hidden-minor-modes
  '(apheleia-mode
    auto-revert-mode
    company-box-mode
    company-mode
    copilot-mode
    eldoc-mode
    eldoc-box-hover-mode
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

(provide 'init-modeline)
;;; init-modeline.el ends here
