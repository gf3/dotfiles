;; init-vertico.el --- General vertico-related configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable vertico
(use-package vertico
  :straight (:host github :repo "minad/vertico"
				   :files ("*.el" "extensions/*.el"))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)

  :init
  (vertico-mode)
  )

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :config
  (keymap-global-set "M-R" #'vertico-repeat)
  (add-to-list 'psession-object-to-save-alist 
               '(vertico-repeat-history . "vertico-repeat-history.el"))
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight (:host github :repo "minad/marginalia")
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :straight (:host github :repo "minad/consult")
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
		 )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight (:host github :repo "oantolin/orderless")
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
		completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Use Orderless as pattern compiler for consult-grep/ripgrep/find
(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (orderless-pattern-compiler input))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input str))))

(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

;; A few more useful configurations...
(use-package emacs
  :init
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

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(provide 'init-vertico)
;;; init-vertico.el ends here
