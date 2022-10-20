;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Log startup time
(defun gf3/display-startup-time ()
  "Display Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
					(time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gf3/display-startup-time)

;; Adjust garbage collector thresholds
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold 16777216 ; 16mb
				  gc-cons-percentage 0.1)))

;; User configs
(let ((custom-path (expand-file-name "init" user-emacs-directory)))
  ;; (eval-when-compile
  ;; 	(add-to-list 'load-path custom-path))
  (add-to-list 'load-path custom-path))

;; Hmm
(add-hook 'after-init-hook
		  (lambda ()
			(progn
			  ;; Packages & configuration
			  (require 'init-execpath)
			  (require 'init-straight)
			  (require 'init-benchmarkinit)
			  (require 'init-direnv)
			  (require 'init-emacs)
			  (require 'init-backup)
			  (require 'init-saveplace)
			  (require 'init-theme)
			  (require 'init-meow)
			  (require 'init-multiplecursors)
			  (require 'init-treesitter)
			  (require 'init-completion)
			  (require 'init-miniframe)
			  (require 'init-buffers)
			  (require 'init-lsp)
			  (require 'init-flycheck)
			  (require 'init-apheleia)
			  (require 'init-whichkey)
			  (require 'init-projectile)
			  (require 'init-magit)
			  (require 'init-alltheicons)
			  (require 'init-modeline)
			  (require 'init-eshell)
			  (require 'init-vterm)
			  (require 'init-browseatremote)
			  (require 'init-movetext)

			  ;; Languages
			  (require 'init-lang-go)
			  (require 'init-lang-graphql)
			  (require 'init-lang-rust)
			  (require 'init-lang-typescript)
			  (require 'init-lang-webmode)
			  (require 'init-lang-yaml)
			  )))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((magit-todos-group-by magit-todos-item-first-path-component magit-todos-item-suffix magit-todos-item-keyword))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(child-frame-border ((t (:background "systemYellowColor")))))
