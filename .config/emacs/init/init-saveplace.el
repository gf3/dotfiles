;;; init-saveplace.el --- Save place in buffers on kill. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))
(save-place-mode 1)

(provide 'init-saveplace)
;;; init-saveplace.el ends here
