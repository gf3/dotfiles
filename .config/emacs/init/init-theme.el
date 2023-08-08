;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package circadian
  :straight t
  :ensure t
  :custom
  (calendar-latitude 43.651070)
  (calendar-longitude -79.347015)
  (circadian-themes '((:sunrise . doom-one-light)
                      (:sunset  . doom-one)))
  :config
  (circadian-setup))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  (doom-themes-padded-modeline t))

(defun gf3/apply-theme (&optional appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (unless appearance (setq appearance 'light))
  (pcase appearance
    ('light (load-theme 'doom-one-light t))
    ('dark (load-theme 'doom-one t))))
;; (set-face-attribute 'mode-line nil
;;                     :box `(:line-width 12 :color ,(face-attribute 'mode-line :background)))
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box `(:line-width 12 :color ,(face-attribute 'mode-line-inactive :background))))

(add-hook 'after-init-hook (lambda () (gf3/apply-theme 'light)))

(add-hook 'ns-system-appearance-change-functions #'gf3/apply-theme)

(provide 'init-theme)
;;; init-theme.el ends here
