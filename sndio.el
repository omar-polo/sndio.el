;;; sndio.el --- Interact with sndio(8)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Version: 1.0
;; Keywords: multimedia
;; URL: https://git.omarpolo.com/sndioctl.el

;;; Commentary:

;; This package provides the sndio major mode to interact with
;; OpenBSD' sndio(8).

;;; Code:

(require 'cl-lib)

(defvar sndioctl-cmd "sndioctl"
  "Path to the sndioctl executable.")

(defvar sndio-step 0.02
  "Step for `sndio-increase' and `sndio-decrease'.")

(define-derived-mode sndio-mode special-mode "sndio"
  "Major mode for sndio interaction."

  (define-key sndio-mode-map (kbd "n") #'forward-line)
  (define-key sndio-mode-map (kbd "p") #'previous-line)
  (define-key sndio-mode-map (kbd "i") #'sndio-increase)
  (define-key sndio-mode-map (kbd "d") #'sndio-decrease)
  (define-key sndio-mode-map (kbd "m") #'sndio-mute)
  (define-key sndio-mode-map (kbd "t") #'sndio-toggle)
  (define-key sndio-mode-map (kbd "g") #'sndio-update)

  (buffer-disable-undo)

  (sndio-update))

(defun sndio-update ()
  "Update the current sndio buffer."
  (interactive)
  (with-current-buffer "*sndio*"
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (process-file sndioctl-cmd nil (current-buffer) nil)))))

(defun sndio--run (&rest args)
  "Run `sndioctl-cmd' with ARGS yielding its output."
  (with-temp-buffer
    (when (zerop (apply #'process-file sndioctl-cmd nil t nil args))
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
;;; sndio.el ends here.
