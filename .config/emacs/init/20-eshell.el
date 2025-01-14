;;; init-eshell.el --- Eshell configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun with-face (str &rest face-plist)
  "Add face properties, FACE-PLIST, to STR."
  (propertize str 'face face-plist))

(defun fish-path (path &optional max-len)
  "Return a potentially trimmed-down version of the directory PATH.
Replacing parent directories with their initial characters to try to
get the character length of PATH (sans directory slashes) down to
MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (max-len (or max-len 30))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun gf3/eshell-prompt ()
  "Custom eshell prompt."
  (concat
   ;; Directory
   (with-face (concat (fish-path (eshell/pwd)) " ") :inherit font-lock-constant-face)
   ;; Git branch
   (if-let* ((git-branch (magit-get-current-branch)))
	   (with-face (concat git-branch " ") :inherit font-lock-string-face))
   ;; Prompt
   (with-face "$" :inherit font-lock-preprocessor-face)
   " "))

(use-package eshell-syntax-highlighting
  :straight (:host github :repo "akreisher/eshell-syntax-highlighting")
  :after eshell
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-vterm
  :straight (:host github :repo "iostapyshyn/eshell-vterm")
  :after eshell
  :config
  (eshell-vterm-mode))

(defun gf3/eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'Z))

(use-package eshell
  :straight t
  :bind ("C-!" . gf3/eshell-new)
  :init
  (setenv "PAGER" "cat")
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'gf3/eshell-prompt)
  (setq eshell-prompt-regexp "^[^$]+ \$ ")
  (setq eshell-buffer-shorthand t)
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top")))
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (eshell/alias "e" "find-file $1")
			  (eshell/alias "emacs" "find-file $1")
			  (eshell/alias "g" "git $*")
			  (eshell/alias "l" "exa -lFh --icons --git --octal-permissions -@ $*")))
  :config
  (defalias 'eshell/v 'eshell-exec-visual)
  (defun eshell/f (&rest args)
	"Find files in the current directory tree."
	(interactive)
	(consult-find))
  (defun eshell/x ()
	"Exit shell."
	(insert "exit")
	(eshell-send-input))
  (defun eshell/z (&optional regexp)
	"Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
	(let ((eshell-dirs (delete-dups
						(mapcar 'abbreviate-file-name
								(ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
		(let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))

(provide 'init-eshell)
;;; init-eshell.el ends here
