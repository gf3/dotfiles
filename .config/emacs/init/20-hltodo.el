;;; init-hltodo.el --- Highlight comment keywords. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :custom
  (hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f") ("TODO" . "#7209b7") ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3") ("PROG" . "#7cb8bb") ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f") ("FAIL" . "#8c5353") ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f") ("MAYBE" . "#d0bf8f") ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f") ("TEMP" . "#d0bf8f") ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")
     
     ("CRITICAL" . "#ff0000") ("IN-PROGRESS" . "#4361ee") ("BLOCKED" . "#4f000b")
     ("WONT-DO" . "#dee2e6")))
  :config
  (global-hl-todo-mode))

(provide 'init-hltodo)
;;; init-hltodo.el ends here
