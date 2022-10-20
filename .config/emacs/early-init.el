;; -*- lexical-binding: t; -*-

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
(set-face-attribute 'default nil :height 130 :family "JetBrains Mono")
(set-face-attribute 'fixed-pitch nil :height 130 :family "JetBrains Mono")
(set-face-attribute 'variable-pitch nil :height 130 :family "Greycliff CF")
