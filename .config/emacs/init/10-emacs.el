;;; 10-emacs.el --- General emacs configuration. -*- lexical-binding: t -*-
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

;; Indent with spaces
(set-default 'indent-tabs-mode nil)

;; Delete selection when pasting, etc...
(delete-selection-mode 1)

;; isearch tweaks
(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-initial-delay 0
 lazy-highlight-buffer t)
(setq-default isearch-wrap-pause 'no)
(setq-default isearch-repeat-on-direction-change t)
(setq-default isearch-lazy-count t)
(setq-default lazy-count-prefix-format nil)
(setq-default lazy-count-suffix-format " (%s/%s)")

;; One Esc is enough
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; See: init-kakoune.el

;; Set cursor shape
(setq-default cursor-type 'bar)

;; Highlight current line
(global-hl-line-mode 1)

;; Simplify yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll like a normal editor
(setq scroll-margin 8
	    scroll-conservatively 1000
	    scroll-up-aggressively nil
	    scroll-down-aggressively nil
	    scroll-preserve-screen-position nil
	    auto-window-vscroll nil)

;; Smooth scrolling
(setq pixel-scroll-precision-use-momentum t)
(pixel-scroll-precision-mode 1)

;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Mouse yank
(global-set-key (kbd "<mouse-2>") 'clipboard-yank)

(setq mouse-autoselect-window t)

(keymap-global-set "<down-mouse-9>" 'strokes-do-stroke)

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
(setq-default tab-width 2)

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
(global-set-key (kbd "S-/") 'hippie-expand)

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

;; Preserve selection after certain operations
(defun gf3/with-mark-active (&rest _args)
  "Keep mark active after command.
To be used as advice AFTER any function that sets `deactivate-mark' to t."
  (setq deactivate-mark nil))

(advice-add 'comment-region :after #'gf3/with-mark-active)
(advice-add 'kill-ring-save :after #'gf3/with-mark-active)

;; Eldoc
(use-package eldoc
  :ensure t
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :custom (eldoc-echo-area-use-multiline-p t)
  :config
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

;; Clear the screen
(add-hook 'shell-command-mode-hook
          (lambda () (local-set-key (kbd "C-K") 'erase-buffer)))

(provide '10-emacs)
;;; 10-emacs.el ends here
