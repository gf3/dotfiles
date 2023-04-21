;;; init-emacs.el --- General emacs configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Misc. config
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t) ; Flash the screen on bell ring
(setq help-window-select t)
(setq read-process-output-max (* 1024 1024 8)) ; Increase amount of data emacs can read from child processes to 8mb

;; Disable native comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

;; More detailed info for completions
(setq completions-detailed t)

;; isearch tweaks
(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-initial-delay 0
 lazy-highlight-buffer t)

;; One Esc is enough
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Highlight current line
(global-hl-line-mode 1)

;; Simplify yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll like a normal editor
(setq scroll-margin 8
	  scroll-conservatively 101
	  scroll-up-aggressively 0.01
	  scroll-down-aggressively 0.01
	  scroll-preserve-screen-position t
	  auto-window-vscroll nil)

;; Smooth scrolling
(setq pixel-scroll-precision-use-momentum t)
(pixel-scroll-precision-mode)

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Mark ring
;; See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark-Ring.html
;; 
;; C-SPC C-SPC
;;     Set mark
;;
;; C-u C-SPC
;;     Jump to last mark
;;
;; C-x C-SPC
;;     Global jump to last mark
(setq set-mark-command-repeat-pop t)
(bind-key "C-x p" 'pop-to-mark-command)

;; Tab width
(setq-default tab-width 4)

;; Auto close brackets
(electric-pair-mode)
(setq electric-pair-inhibit-predicate 'ignore)
(setq electric-pair-skip-self t)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Autocomplete word
(global-set-key (kbd "M-/") 'hippie-expand)

;; Compilation scrolling
(setq compilation-scroll-output 'first-error)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
	  #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(provide 'init-emacs)
;;; init-emacs.el ends here
