;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Gianni Chiappetta"
      user-mail-address "gianni@runlevel6.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font                (font-spec :family "JetBrains Mono" :size 14 :weight 'light :height 1.3)
      doom-variable-pitch-font (font-spec :family "Greycliff CF" :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-snazzy)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (list "inbox.org" "diary.org"))

;; File templates
(set-file-template! "RFCs/.*\\.org$" :trigger "__rfc.org" :mode 'org-mode)

;; Scrolling
(setq scroll-margin 10)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; GUI Emacs
(setq frame-resize-pixelwise t)

;; Evil cutting and pasting
(setq evil-v$-excludes-newline t
      evil-kill-on-visual-paste nil
      select-enable-clipboard nil)

(map! :desc "Clipboard commands"
      (:when IS-MAC
        :v "s-c" 'clipboard-kill-ring-save
        :g "s-v" 'clipboard-yank))

;; Global regex by default
(setq evil-ex-substitute-global t)

;; Mixed pitch fonts
(use-package! mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; LSP
(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-imenu-auto-refresh t
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-mode 1))

;; String inflection
(use-package! string-inflection
  :config
  (define-key global-map (kbd "C-c ~") 'string-inflection-all-cycle))

;; HCL
(use-package! hcl-mode)

;; ibuffer VC
(use-package! ibuffer-vc
  :after ibuffer)

(use-package! ox-gfm
  :after org)

(use-package! org-super-agenda
  :after org
  :config
  (setq org-super-agenda-groups
       '((:log t)  ; Automatically named "Log"
         (:name "Schedule"
                :time-grid t)
         (:name "Today"
                :scheduled today)
         (:habit t)
         (:name "Due today"
                :deadline today)
         (:name "Overdue"
                :deadline past)
         (:name "Due soon"
                :deadline future)
         (:name "Unimportant"
                :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                :order 100)
         (:name "Waiting..."
                :todo "WAITING"
                :order 98)
         (:name "Scheduled earlier"
                :scheduled past))))

(defun gf3/capture-report-date-file ()
  "Creates a filename from a prompt and includes a date"
  (interactive)
  (let ((name (read-string "Name: ")))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y-%m-%d")
             name)
     (concat org-directory "RFCs"))))

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Documents/org/people.org")))

(defun gf3/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

;; Org config
(after! org
  ;; Disable indent
  (org-indent-mode -1)

  ;; Visual line mode
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%a, %Y-%m-%d"
        org-journal-file-format "%Y-%m-%d.org")

  ;; Agenda
  (setq org-agenda-include-diary t)

  ;; Text
  (add-hook 'org-mode-hook (lambda () (text-scale-increase 1)))
  (setq
   ;; Edit settings
   org-auto-align-tags t
   ;; org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   ;; org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities nil
   org-ellipsis " ↓")

  ;; Fonts
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-title ((t (:height 650 :font "Wayfinder CF" :spacing 50 :weight bold :underline nil))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-level-1 ((t (:inherit nil :weight thin))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-todo ((t (:inherit nil :font "Ellograph CF" :height 0.8 :weight bold))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8)))))

  ;; Templates
  (defvar gf3/org-contacts-template "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Front St., Toronto ON, M4P 2L5}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")

  ;; Org
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %? %^G\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n")
           :empty-lines 1)
          ("p" "Person" entry (file+headline "people.org" "People"),
           gf3/org-contacts-template
           :empty-lines 1)
          ("r" "RFC" plain (function gf3/capture-report-date-file)
           ,(concat "#+TITLE: %^{RFC Title} %^G\n"
                    "#+AUTHOR: %n\n"
                    "#+DESCRIPTION: %^{RFC Description}\n"
                    "#+CREATED: %U\n"
                    "#+KEYWORDS: rfc\n\n"
                    "* Problem\n\n"
                    "%?\n\n"
                    "* Solution\n\n"
                    "* Implementation\n")
           :jump-to-captured t)))


  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))

  (define-key global-map (kbd "C-c i") 'org-capture-inbox))

;; Org Caldav
(use-package! org-caldav
  :ensure t
  :config
  (setq org-caldav-url "https://caldav.fastmail.com/dav/calendars/user/gianni@runlevel6.org/"
        org-caldav-calendars '((:calendar-id "1cf72173-d221-4717-a5ed-04c6ae1b0a65"
                                :inbox "~/Documents/org/calendar-fastmail.org"
                                :files ()))
        org-icalendar-timezone "America/Toronto"))

;; Battery
(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

;; Time
(display-time-mode 1)

(defadvice! prompt-for-buffer (&rest _)
  :after 'window-split (switch-to-buffer))

;; History
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Indent
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

;; Match terminal theme with emacs theme
(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))

;; LOL
(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

;; Underscores
(setq-default evil-symbol-word-search t)
(add-hook 'after-change-major-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; Magit repositories
(setq magit-repository-directories '("~/Code/github.com/"))

;; Move stuff around with arrow keys
(use-package! drag-stuff
  :after evil
  :config
  (drag-stuff-global-mode 1)
  (when (featurep! :editor meow)
   (meow-define-keys 'normal
     '("<up>" . drag-stuff-up)
     '("<down>" . drag-stuff-down)))

  (when (featurep! :editor evil)
   ;; Drag lines up
   (define-key evil-normal-state-map (kbd "<up>") 'drag-stuff-up)
   (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
   ;; Drag lines down
   (define-key evil-normal-state-map (kbd "<down>") 'drag-stuff-down)
   (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)))

;; Link to git things
(use-package! git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link))

;; Generate go tests
(use-package! gotests)

;; Go Tmpl files
(use-package! web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-engines-alist
        '(("go"    . "\\.html\\'")
          ("erb"   . "\\.plush\\.html\\'")
          ("go"    . "\\.tmpl\\'"))))
