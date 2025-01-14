;;; init-meow.el --- Modal editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gf3/meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   ;; 'j' and 'k' are suggested to be bound to `meow-next' and `meow-prev', but
   ;; these deactivate the region, which is not helpful unless we're in normal
   ;; state.
   ;; TODO PR in suggested bindings
   '("j" . next-line)
   '("k" . previous-line)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("SPC" . meow-M-x)
   ;; '("<left>" . previous-buffer)
   ;; '("<right>" . next-buffer)
   ;; ;; The suggested bindings would have allowed us to use 'SPC j' and 'SPC k'
   ;; ;; to run whatever commands were originally mapped to 'j' and 'k' while in
   ;; ;; Motion state. But our bindings make SPC k the prefix for
   ;; ;; `doom-leader-code-map', so that won't work. Nevertheless, we leave
   ;; ;; those bindings here anyway.
   ;; SPC j/k will run the original command in MOTION state
   ;; (because we set `meow-motion-remap-prefix' to "C-")
   '("j" . "C-j")
   '("k" . "C-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
                                        ;'("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   ;; `yank-pop' is pretty essential, and 'P' is convenient and not used.
   ;; TODO PR in suggested bindings
   '("P" . meow-yank-pop)
   ;; Integrate with Doom's popup system instead of calling `meow-quit'.
   '("q" . nil)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . delete-region)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :straight (:host github :repo "meow-edit/meow" :branch "master")
  :config
  (setq meow-goto-line-function 'consult-line)
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))
  (meow-setup-indicator)
  (gf3/meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :straight (:host github :repo "skissue/meow-tree-sitter" :branch "main")
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'init-meow)
;;; init-meow.el ends here
