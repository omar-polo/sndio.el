;;; sndio.el --- Interact with sndio(8)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Version: 1.0
;; Keywords: multimedia
;; URL: https://git.omarpolo.com/sndio.el

;;; Commentary:

;; This package provides the sndio major mode to interact with
;; OpenBSD' sndio(8).

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar sndio-sndioctl-cmd "sndioctl"
  "Path to the sndioctl executable.")

(defvar sndio-step 0.02
  "Step for `sndio-increase' and `sndio-decrease'.")

(defvar sndio-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") #'forward-line)
    (define-key m (kbd "p") #'previous-line)
    (define-key m (kbd "i") #'sndio-increase)
    (define-key m (kbd "d") #'sndio-decrease)
    (define-key m (kbd "m") #'sndio-mute)
    (define-key m (kbd "t") #'sndio-toggle)
    (define-key m (kbd "g") #'sndio-update)
    m)
  "Keymap for sndio.")

(define-derived-mode sndio-mode special-mode "sndio"
  "Major mode for sndio interaction."
  (buffer-disable-undo)
  (sndio-update))

(defun sndio-update ()
  "Update the current sndio buffer."
  (interactive)
  (with-current-buffer "*sndio*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (process-file sndio-sndioctl-cmd nil (current-buffer) nil))))

(defun sndio--run (&rest args)
  "Run `sndio-sndioctl-cmd' with ARGS yielding its output."
  (with-temp-buffer
    (when (zerop (apply #'process-file sndio-sndioctl-cmd nil t nil args))
      (buffer-string))))

(defun sndio--current-io ()
  "Yield the input/poutput at point as string."
  (when-let (end (save-excursion
                   (beginning-of-line)
                   (ignore-errors (search-forward "="))))
    (buffer-substring-no-properties (line-beginning-position)
                                    (1- end))))

(defun sndio--update-value (x)
  "Update the value for the input/output at point setting it to X."
  (save-excursion
    (beginning-of-line)
    (search-forward "=")
    (let ((inhibit-read-only t))
      (delete-region (point) (line-end-position))
      (insert (string-trim-right x)))))

(defun sndio-increase ()
  "Increase the volume for the input/output at point."
  (interactive)
  (when-let (x (sndio--current-io))
    (when-let (val (sndio--run "-n" (concat x "=+" (number-to-string sndio-step))))
      (sndio--update-value val))))

(defun sndio-decrease ()
  "Decrease the volume for the input/output at point."
  (interactive)
  (when-let (x (sndio--current-io))
    (when-let (val (sndio--run "-n" (concat x "=-" (number-to-string sndio-step))))
      (sndio--update-value val))))

(defun sndio-mute ()
  "Mute the input/output at point."
  (interactive)
  (when-let (x (sndio--current-io))
    (when-let (val (sndio--run "-n" (concat x "=0")))
      (sndio--update-value val))))

(defun sndio-toggle ()
  "Toggle input/output at point."
  (interactive)
  (when-let (x (sndio--current-io))
    (when-let (val (sndio--run "-n" (concat x "=!")))
      (sndio--update-value val))))

;;;###autoload
(defun sndio ()
  "Launch sndio."
  (interactive)
  (switch-to-buffer "*sndio*")
  (sndio-mode))

(provide 'sndio)
;;; sndio.el ends here
