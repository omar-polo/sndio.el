;;; sndio.el --- Interact with sndio(8)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021 Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Version: 1.0
;; Keywords: multimedia
;; URL: https://git.omarpolo.com/sndio.el
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the sndio major mode to interact with
;; OpenBSD' sndio(8).

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar sndio-sndioctl-cmd "sndioctl"
  "Path to the sndioctl executable.")

(defvar sndio-step 0.02
  "Step for `sndio-increase' and `sndio-decrease'.")

(defvar sndio--window nil
  "The sndio window.")

(defvar sndio-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") #'forward-line)
    (define-key m (kbd "p") #'previous-line)
    (define-key m (kbd "i") #'sndio-increase)
    (define-key m (kbd "d") #'sndio-decrease)
    (define-key m (kbd "m") #'sndio-mute)
    (define-key m (kbd "t") #'sndio-toggle)
    (define-key m (kbd "g") #'sndio-update)
    (define-key m (kbd "q") #'sndio-quit)
    m)
  "Keymap for sndio.")

(define-derived-mode sndio-mode special-mode "sndio"
  "Major mode for sndio interaction."
  (buffer-disable-undo)
  (sndio-update))

(defun sndio-update ()
  "Update the current sndio buffer."
  (interactive)
  (when (derived-mode-p 'sndio-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (process-file sndio-sndioctl-cmd nil (current-buffer) nil)
      (goto-char (point-min)))))

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

(defun sndio-quit ()
  "Quits sndio.
Call `delete-window' when the sndio popup window is open or
`quit-window' otherwise."
  (interactive)
  (if (window-live-p sndio--window)
      (delete-window)
    (quit-window)))

;;;###autoload
(defun sndio ()
  "Launch sndio."
  (interactive)
  (switch-to-buffer "*sndio*")
  (sndio-mode))

;;;###autoload
(defun sndio-win-open ()
  "Open an sndio window at the bottom of the frame for quick editing."
  (interactive)
  (unless (window-live-p sndio--window)
    (setq sndio--window
          (select-window
           (let ((ignore-window-parameters t))
             (split-window (frame-root-window)
                           -1
                           'below))
           'norecord))
    (switch-to-buffer (get-buffer-create "*sndio-quick*")
                      'norecord)
    (if (derived-mode-p 'sndio-mode)
        (sndio-update)
      (sndio-mode))
    (hl-line-mode +1)
    (setq mode-line-format nil
          header-line-format nil
          tab-line-format nil)
    (set-window-hscroll sndio--window 0)
    (set-window-dedicated-p sndio--window t)
    (select-window sndio--window 'norecord)
    (let ((window-resize-pixelwise t)
          (window-size-fixed))
      (fit-window-to-buffer sndio--window nil nil 1))))

(provide 'sndio)
;;; sndio.el ends here
