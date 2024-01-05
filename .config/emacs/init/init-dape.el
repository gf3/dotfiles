;;; init-dape.el --- Debugger. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dape
  :straight (dape :type git :host github :repo "svaante/dape")
  ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangement 'gud)
  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn (lambda (&optional skip-tramp-trim)
                      (let ((root (projectile-project-root)))
                        (if (and (not skip-tramp-trim) (tramp-tramp-file-p root))
                            (tramp-file-name-localname (tramp-dissect-file-name root))
                          root)))))

(provide 'init-dape)
;;; init-dape.el ends here
