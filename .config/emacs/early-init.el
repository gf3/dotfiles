;;; early-init.el --- Early configuration init -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Prefer newer source files over older byte-compiled files
(setq load-prefer-newer t)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "echo $E:PATH"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)

;; Don't fuck with my init.el file
(setq package--init-file-ensured t)

;; Set the frame to rounded
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Resize the frame by pixels instead of cols/rows
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Set font
(defun gf3/set-font ()
  "Set default fonts."
  (set-face-attribute 'default nil :height 150 :font "JetBrainsMono Nerd Font" :weight 'light)
  (set-face-attribute 'fixed-pitch nil :height 150 :font "JetBrainsMono Nerd Font" :weight 'light)
  (set-face-attribute 'variable-pitch nil :height 165 :font "Greycliff CF")
  (set-frame-font "JetBrainsMono Nerd Font Light 15"))

(gf3/set-font)

;;; early-init.el ends here
