;;; init-modeline.el --- Mode line configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package doom-modeline
;;   :straight t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 32)
;;   (setq doom-modeline-hud t))

(defcustom mood-line-kakoune-state-alist
  '((normal . ("<N>" . font-lock-variable-name-face))
    (insert . ("<I>" . font-lock-string-face)))
  "Set the string and corresponding face for each kakoune state."
  :group 'mood-line
  :type '(alist
          :key-type symbol
          :value-type (cons (string :tag "Display Text")
                            (choice :tag "Face" face plist))))

(use-package mood-line
  :straight t
  :config
  (mood-line-mode)
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line-inactive :background)))
  (setq mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
