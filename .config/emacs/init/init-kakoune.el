;;; init-kakoune.el --- Kakoune-like editing experience in emacs. -*- lexical-binding: t -*-

;; Author: Gianni Chiappetta
;; Maintainer: Gianni Chiappetta
;; Version: version
;; Package-Requires: (kakoune-mode ryo-modal-mode multiple-cursors.el expand-region.el)
;; Homepage: homepage
;; Keywords: editing, modal, kakoune


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package expand-region
  :straight t)

(use-package multiple-cursors
  :straight t)

(defun kakoune-delete (count)
  "Delete selected text or COUNT chars."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char count t)))

(use-package kakoune
  :straight t
  :demand t
  :bind ("C-z" . ryo-modal-mode)
  :config
  (global-set-key (kbd "<escape>") (lambda ()
                                     (interactive)
                                     (if (boundp 'ryo-modal-mode)
                                         (if ryo-modal-mode
                                             (keyboard-escape-quit)
                                           (ryo-modal-mode 1))
                                       (keyboard-escape-quit))))
  (defun ryo-enter () "Enter normal mode" (interactive) (ryo-modal-mode 1))
  (kakoune-setup-keybinds)
  (setq ryo-modal-cursor-type 'box)
  (add-hook 'prog-mode-hook #'ryo-enter)
  ;; Access all C-x bindings easily
  (define-key ryo-modal-mode-map (kbd "SPC") (lookup-key global-map (kbd "C-c")))
  (global-set-key (kbd "C-c SPC") '("M-x" . execute-extended-command))
  (global-set-key (kbd "C-c x") `("C-x" . ,(lookup-key global-map (kbd "C-x"))))
  (global-set-key (kbd "C-c ;") '("Toggle comment" . comment-or-uncomment-region))
  (global-set-key (kbd "C-c h") '("Help" . help-command))

  (ryo-modal-major-mode-keys
   'prog-mode
   ("b" kakoune-backward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
   ("B" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t)
   ("w" forward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
   ("W" forward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t))
  (ryo-modal-keys
   ("*" mc/mark-all-like-this)
   ("," save-buffer)
   ("d" kakoune-delete)
   ("e" forward-symbol :first '(kakoune-set-mark-here))
   ("E" backward-word :first '(kakoune-set-mark-here))
   ("m" mc/mark-next-like-this)
   ("M" mc/skip-to-next-like-this)
   ("M-m" mc/edit-lines)
   ("n" mc/mark-previous-like-this)
   ("N" mc/skip-to-previous-like-this)
   ("P" consult-yank-pop)
   ("v" er/expand-region)
   ("x" kakoune-X)
   ("C-v" set-rectangular-region-anchor)
   ("M-s" mc/split-region)
   (";" (("q" delete-window :name "Delete window")
         ("v" split-window-horizontally :name "Split window horizontally")
         ("s" split-window-vertically :name "Split window vertically")))
   ("C-h" windmove-left)
   ("C-j" windmove-down)
   ("C-k" windmove-up)
   ("C-l" windmove-right)
   ("C-u" scroll-down-command :first '(deactivate-mark))
   ("C-d" scroll-up-command :first '(deactivate-mark))
   ("<up>" move-text-up :name "Move line up")
   ("<down>" move-text-down :name "Move line down")))

;; This overrides the default mark-in-region with a prettier-looking one,
;; and provides a couple extra commands
(use-package visual-regexp
  :straight t
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace))

;; Add/change/delete pairs of delimiters
(use-package embrace
  :straight (:host github :repo "cute-jumper/embrace.el")
  :ryo
  ("S" embrace-commander))

;; Probably the first thing you'd miss is undo and redo, which requires an extra package
;; to work like it does in kakoune (and almost every other editor).
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "~/.config/emacs/tmp/undo"))))
  (undo-tree-visualizer-timestamps t)
  :ryo
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("SPC u" undo-tree-visualize)
  :bind (:map undo-tree-visualizer-mode-map
              ("h" . undo-tree-visualize-switch-branch-left)
              ("j" . undo-tree-visualize-redo)
              ("k" . undo-tree-visualize-undo)
              ("l" . undo-tree-visualize-switch-branch-right)))


(provide 'init-kakoune)

;;; init-kakoune.el ends here
