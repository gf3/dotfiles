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
      doom-variable-pitch-font (font-spec :family "Merriweather" :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-snazzy)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

;; Meow (modal editing)
(define-key global-map (kbd "C-c `") 'meow-last-buffer)
(define-key global-map (kbd "C-c B") 'consult-project-buffer)
(define-key global-map (kbd "C-x C-b") 'projectile-ibuffer)

(map! :leader
      ;; make doom-leader-buffer-map alive
      (:prefix-map ("b" . "buffer")
       :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       (:when (featurep! :ui workspaces)
        :desc "Switch workspace buffer"    "b" #'persp-switch-to-buffer
        :desc "Switch buffer"              "B" #'switch-to-buffer)
       (:unless (featurep! :ui workspaces)
        :desc "Switch buffer"               "b"   #'switch-to-buffer)
       :desc "Clone buffer"                "c"   #'clone-indirect-buffer
       :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "New empty buffer"            "N"   #'+default/new-buffer
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers))


(setq doom-localleader-alt-key "C-l")
(map! :map meow-leader-keymap
  "l" doom-localleader-alt-key)

(defun gf3/meow-append-line ()
  "Enter insert mode at the end of a line"
  (interactive)
  (meow-line 0)
  (meow-insert))

(after! meow
  (defun meow--post-isearch-function ()
    (unless isearch-mode-end-hook-quit
      (when isearch-success
        (let ((beg (car isearch-match-data))
              (end (cadr isearch-match-data)))

          (thread-first
            (meow--make-selection '(select . visit)
                                  beg
                                  (if isearch-forward end isearch-other-end))
            (meow--select (not isearch-forward)))))))

  (add-hook 'isearch-mode-end-hook 'meow--post-isearch-function)

  (setq
   meow-use-cursor-position-hack t
   meow-use-enhanced-selection-effect t
   meow-use-clipboard t)

  (meow-define-keys
      'normal

    '("/" . isearch-forward)
    '("?" . meow-query-replace-regexp)
    '("<tab>" . indent-for-tab-command)
    '("<<" . doom/dumb-dedent)
    '(">>" . doom/dumb-indent)
    '("A" . gf3/meow-append-line)
    '("O" . meow-open-below)
    '("V" . consult-imenu)))

;; ibuffer VC
(use-package! ibuffer-vc
  :after ibuffer)

(use-package! ox-gfm
  :after org)

(use-package! org-super-agenda
  :after org)

(use-package! org-modern
  :after org
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  ;; Enable org-modern
  (global-org-modern-mode)

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(defun gf3/capture-report-date-file ()
  "Creates a filename from a prompt and includes a date"
  (interactive)
  (let ((name (read-string "Name: ")))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y-%m-%d")
             name)
     (concat org-directory "RFCs"))))

;; Org config
(after! org
  ;; Disable indent
  (org-indent-mode -1)

  ;; Journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%a, %Y-%m-%d"
        org-journal-file-format "%Y-%m-%d.org")

  ;; Agenda
  (setq org-agenda-include-diary t)

  ;; Org
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %? %^G\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n")
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
  :after meow
  :config
  (drag-stuff-global-mode 1)

    (meow-define-keys
      'normal

    '("<up>" . drag-stuff-up)
    '("<down>" . drag-stuff-down)))

;; Match-it!
(use-package! evil-matchit
  :after meow
  :config
  (meow-define-keys
      'normal

    '("%" . evilmi-jump-items-native)))

;; Link to git things
(use-package! git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link))

;; Generate go tests
(use-package! gotests)

;; Auto minor modes
(add-to-list 'auto-minor-mode-alist '("\\.(go|[jt]sx?)$" . minimap-mode))

;; Go Tmpl files
(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("go"    . "\\.html\\'")
          ("go"    . "\\.tmpl\\'"))))
