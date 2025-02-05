;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Log startup time
(defun gf3/display-startup-time ()
  "Display Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
					          (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gf3/display-startup-time)

;; (setq debug-on-error t)

;; Adjust garbage collector thresholds
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
		      (lambda ()
			      (setq gc-cons-threshold 16777216 ; 16mb
				          gc-cons-percentage 0.1)))

;; Custom themes
(setq custom-safe-themes t)

;; Fonts
(require 'cl-lib)

(defun gf3/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defun gf3/preferred-font-size ()
  "Calculate the preferred font size based on the monitor DPI."
  (let ((dpi (gf3/get-dpi)))
    (cond
     ((< dpi 110) 11)
     ((< dpi 130) 11)
     ((< dpi 160) 11)
     (t 11))))

(defvar fixed-pitch-font-name "Maple Mono NF" "The fixed pitch font name.")
(defvar variable-pitch-font-name "Iosevka Aile" "The variable pitch font name.")
(defvar preferred-font-size (gf3/preferred-font-size) "The preferred font size.")
(defvar preferred-font (format "%s-%d:weight=regular" fixed-pitch-font-name preferred-font-size) "The preferred font.")

(defun gf3/set-fonts ()
  "Set the default fonts."
  (message "Setting fonts: dpi: %d, size: %d, fixed: %s, variable: %s" (gf3/get-dpi) preferred-font-size fixed-pitch-font-name variable-pitch-font-name)
  (set-frame-font preferred-font t t)
  (set-face-font 'fixed-pitch-serif fixed-pitch-font-name)
  (set-face-font 'variable-pitch variable-pitch-font-name))

;; User configs
(let ((custom-path (expand-file-name "init" user-emacs-directory)))
  (add-to-list 'load-path custom-path))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(defun gf3/load-directory (dir)
  "Load all .el files in DIR."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; Load 'em up!
(gf3/load-directory (expand-file-name "init" user-emacs-directory))

;; Font
(gf3/set-fonts)

;;; init.el ends here
(put 'erase-buffer 'disabled nil)
