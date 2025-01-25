;;; init-meow.el --- Modal editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; See: http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html
(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Version: 2020-06-26 2023-11-14"
  (interactive)
  (let ( (deactivate-mark nil) xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq xp1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq xp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xp1 xp2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xp1 xp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xp1 xp2)
      (put this-command 'state 0)))))

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
   (cons "p" project-prefix-map)
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
   '("s" . meow-kill)
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
   '("~" . xah-toggle-letter-case)
   '("<escape>" . ignore)
   '("<up>" . ignore)
   '("<down>" . ignore)))

(use-package meow
  :demand t
  :straight (:host github :repo "meow-edit/meow" :branch "master")
  :config
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
