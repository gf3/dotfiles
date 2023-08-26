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

;; Fonts
(require 'cl-lib)

(defun gf3/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defun gf3/preferred-font-size ()
  "Calculate the preferred font size based on the monitor DPI."
  (let ((dpi (gf3/get-dpi)))
    (cond
     ((< dpi 110) 13)
     ((< dpi 130) 14)
     ((< dpi 160) 15)
     (t 15))))

(defvar fixed-pitch-font-name "Source Code Pro" "The fixed pitch font name.")
(defvar variable-pitch-font-name "GreycliffCF Nerd Font" "The variable pitch font name.")
(defvar preferred-font-size (gf3/preferred-font-size) "The preferred font size.")
(defvar preferred-font (format "%s-%d:weight=regular" fixed-pitch-font-name preferred-font-size) "The preferred font.")

(defun gf3/set-fonts ()
  "Set the default fonts."
  (message "Setting fonts: dpi: %d, size: %d, fixed: %s, variable: %s" (gf3/get-dpi) preferred-font-size fixed-pitch-font-name variable-pitch-font-name)
  (set-frame-font preferred-font t t)
  (set-face-font 'fixed-pitch-serif fixed-pitch-font-name)
  (set-face-font 'variable-pitch variable-pitch-font-name))

;; User configs
(let ((custom-path (expand-file-name "init" user-emacs-directory)))
  ;; (eval-when-compile
  ;; 	(add-to-list 'load-path custom-path))
  (add-to-list 'load-path custom-path))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; Hmm
(add-hook 'after-init-hook
		  (lambda ()
			(progn
			  ;; Packages & configuration
			  (require 'init-straight)
			  (require 'init-benchmarkinit)
			  (require 'init-direnv)
			  (require 'init-psession)
			  (require 'init-emacs)
			  (require 'init-perspective)
			  (require 'init-backup)
			  (require 'init-saveplace)
			  (require 'init-theme)
			  (require 'init-embrace)
			  (require 'init-multiplecursors)
			  (require 'init-treesitter)
			  (require 'init-company)
			  (require 'init-vertico)
			  (require 'init-anzu)
			  (require 'init-buffers)
			  (require 'init-deadgrep)
			  (require 'init-popwin)
			  (require 'init-hltodo)
			  (require 'init-eglot)
			  (require 'init-flycheck)
			  (require 'init-apheleia)
			  (require 'init-projectile)
			  (require 'init-magit)
			  (require 'init-diffhl)
			  (require 'init-alltheicons)
			  (require 'init-modeline)
			  (require 'init-eshell)
			  (require 'init-vterm)
			  (require 'init-movetext)
			  (require 'init-beacon)
			  (require 'init-helpful)
			  (require 'init-yasnippet)
			  (require 'init-codereview)
			  (require 'init-writegood)
			  (require 'init-writeroom)
			  (require 'init-copilot)
              (require 'init-whichkey)
              (require 'init-kakoune)

			  ;; Languages
			  (require 'init-lang-docker)
			  (require 'init-lang-elixir)
			  (require 'init-lang-elvish)
			  (require 'init-lang-go)
			  (require 'init-lang-graphql)
			  (require 'init-lang-odin)
			  (require 'init-lang-rust)
              (require 'init-lang-terraform)
			  (require 'init-lang-typescript)
			  (require 'init-lang-webmode)
			  (require 'init-lang-yaml)
			  (require 'init-lang-zig)

			  ;; Font
              (gf3/set-fonts)
              (circadian-setup)
			  )))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " → ")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes
   '("904ccc456f6be7860252f4bf47dab4bbe684328a925749a3cd11fab8faf4d8d0"
     "a8970b307ece3e37d7d56df11a7199733793b6a8482e1010dc40b027b4994183"
     "1e3243439aa654f07302277b43fb741619903a7b67993c0fb76fcb1db7b42924"
     "2007ae44334eda7781d3d17a6235cd2d7f236e1b8b090e33c8e7feb74c92b634"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64"
     "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563"
     "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     default))
 '(mood-line-meow-state-alist
   '((normal "🅽" . font-lock-variable-name-face)
     (insert "🅸" . font-lock-string-face)
     (keypad "🅺" . font-lock-keyword-face)
     (beacon "🅱" . font-lock-type-face)
     (motion "🅼" . font-lock-constant-face)))
 '(safe-local-variable-values
   '((js-indent-level 0.2)
     (projectile-root-local .
                            "/home/gianni/Code/github.com/freshlineapp/wharf")
     (js-indent-level . 2) (web-mode-code-indent . 2)
     (eval prettier-mode t)
     (magit-todos-group-by magit-todos-item-first-path-component
                           magit-todos-item-suffix
                           magit-todos-item-keyword))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
