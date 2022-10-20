;;; init-browseatremote.el --- Browse at remote configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package browse-at-remote
  :straight (:host github :repo "rmuslimov/browse-at-remote")
  :defer t
  :commands (browse-at-remote bar-browse)
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(provide 'init-browseatremote)
;;; init-browseatremote.el ends here
