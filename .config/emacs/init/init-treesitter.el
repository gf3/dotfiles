;;; init-treesitter.el --- Tree sitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :straight (:host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :config
  (add-to-list 'treesit-extra-load-path (expand-file-name "ts-grammars" user-emacs-directory)))

(use-package tree-sitter-langs
  :straight (:host github :repo "emacs-tree-sitter/tree-sitter-langs" :pre-build '(("git" "submodule" "update" "--remote" "--merge" "repos/elixir/")))
  :after tree-sitter)

(defun my/tree-sitter-compile-grammar (destination &optional path)
  "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
  (interactive "fWhere should we put the shared library? \nfWhat tree-sitter grammar are we compiling? \n")
  (make-directory destination 'parents)

  (let* ((default-directory
          (expand-file-name "src/" (or path default-directory)))
         (parser-name
          (thread-last (expand-file-name "grammar.json" default-directory)
                       (json-read-file)
                       (alist-get 'name)))
         (emacs-module-url
          "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/emacs-module.h")
         (tree-sitter-lang-in-url
          "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/tree-sitter-lang.in")
         (needs-cpp-compiler nil))
    (message "Compiling grammar at %s" path)

    (url-copy-file emacs-module-url "emacs-module.h" :ok-if-already-exists)
    (url-copy-file tree-sitter-lang-in-url "tree-sitter-lang.in" :ok-if-already-exists)

    (with-temp-buffer
      (unless
          (zerop
           (apply #'call-process
                  (if (file-exists-p "scanner.cc") "c++" "cc") nil t nil
                  "parser.c" "-I." "--shared" "-o"
                  (expand-file-name
                   (format "libtree-sitter-%s%s" parser-name module-file-suffix)
                   destination)
                  (cond ((file-exists-p "scanner.c") '("scanner.c"))
                        ((file-exists-p "scanner.cc") '("scanner.cc")))))
        (user-error
         "Unable to compile grammar, please file a bug report\n%s"
         (buffer-string))))
    (message "Completed compilation")))

(use-package tree-sitter-dockerfile
  :defer t
  :straight (:host github
				   :repo "camdencheek/tree-sitter-dockerfile"
				   :post-build
				   (my/tree-sitter-compile-grammar
					(expand-file-name "ts-grammars" user-emacs-directory))))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
