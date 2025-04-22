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

;; (setq debug-on-error t)

;; Adjust garbage collector thresholds
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
		      (lambda ()
			      (setq gc-cons-threshold 16777216 ; 16mb
				          gc-cons-percentage 0.1)))

;; Custom themes
(setq custom-safe-themes t)

;; Fonts
(defvar fixed-pitch-font-name "Maple Mono NF" "The fixed pitch font name.")
(defvar variable-pitch-font-name "Iosevka Aile" "The variable pitch font name.")
(defvar preferred-font-size 13 "The preferred font size.")
(defvar preferred-font (format "%s-%d:weight=regular" fixed-pitch-font-name preferred-font-size) "The preferred font.")

(defun gf3/set-fonts ()
  "Set the default fonts."
  (message "Setting fonts: size: %d, fixed: %s, variable: %s" preferred-font-size fixed-pitch-font-name variable-pitch-font-name)
  (set-frame-font preferred-font t t)
  (set-face-font 'fixed-pitch-serif fixed-pitch-font-name)
  (set-face-font 'variable-pitch variable-pitch-font-name))

;; User configs
(let ((custom-path (expand-file-name "init" user-emacs-directory)))
  (add-to-list 'load-path custom-path))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(defun gf3/load-directory (dir)
  "Load all .el files in DIR."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; Load 'em up!
(gf3/load-directory (expand-file-name "init" user-emacs-directory))

;; Font
(gf3/set-fonts)

;; Misc.
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
