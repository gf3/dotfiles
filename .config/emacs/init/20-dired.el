(use-package all-the-icons-dired
  :straight (all-the-icons-dired :host github :repo "jtbm37/all-the-icons-dired")
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("c" "~/Code"                          "Code")
     ("f" "~/Code/github.com/freshlineapp", "Freshline")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  (dirvish-side-width 50)
  (dirvish-side-open-file-action 'mru)
  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
                 "--group-directories-first" ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'exa)


  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package diredfl
  :straight t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dired-sidebar
  :bind (([f9] . +sidebar-toggle))
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face `(:family "Inter Variable" :height 120)))

(use-package ibuffer-sidebar
  :straight t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Inter Variable" :height 120)))

(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))
