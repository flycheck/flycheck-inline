;;; flycheck-inline.el --- Display Flycheck errors inline -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 fmdkdd

;; Author: fmdkdd
;; URL: https://github.com/flycheck/flycheck-inline
;; Keywords: tools, convenience
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "25.1") (flycheck "32"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide an error display function to show Flycheck errors inline, directly
;; below their location in the buffer.
;;
;; # Setup
;;
;; Enable the local minor mode for all flycheck-mode buffers:
;;
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;; Code:

(require 'flycheck)
(require 'seq)


;;; Displaying line-long overlays (phantoms)

(defun flycheck-inline-phantom-display (msg &optional pos err)
  "Display MSG in a phantom directly below POS.

MSG is a string that will be put in a line-long overlay (phantom)
at the line immediately following POS.  If POS is nil, current
point is used instead.

Return the displayed phantom."
  (pcase-let* ((p (or pos (point)))
               (`(,offset . ,pos-eol)
                (save-excursion
                  (goto-char p)
                  (cons (- p (point-at-bol)) (point-at-eol))))
               (ov (make-overlay pos-eol (1+ pos-eol)))
               ;; If the error is on the last line, and that line doesn't end
               ;; with a newline, the overlay will be displayed at the end of
               ;; the line instead of below it.  Adding a newline before the
               ;; message fixes it.
               (str (concat (when (eq pos-eol (point-max)) "\n")
                            (flycheck-inline-indent-message offset msg)
                            "\n")))
    (overlay-put ov 'phantom t)
    (overlay-put ov 'after-string str)
    (overlay-put ov 'error err)
    ov))

(defun flycheck-inline--contains-point (phantom &optional pt)
  "Whether the given error overlay contains the position PT otherwise `(point)'"
  (let* ((pos (or pt (point)))
         (err (overlay-get phantom 'error))
         (region (flycheck-error-region-for-mode err 'symbols)))
    (and phantom
         ;; Must be one of our phantoms (probably unneeded).
         (overlay-get phantom 'phantom)
         ;; The underlying error must currently exist.
         err
         (memq err flycheck-current-errors)
         ;; Most importantly, point must be within the error bounds.
         region
         (>= pos (car region))
         (<= pos (cdr region)))))

(defun flycheck-inline-phantom-delete (phantom)
  "Delete PHANTOM if its region doesn't contain point.

Returns the overlay removed or nil."
  (if (flycheck-inline--contains-point phantom)
      nil
    (progn (delete-overlay phantom) t)))

(defun flycheck-inline-indent-message (offset msg)
  "Indent all lines of MSG by OFFSET spaces.

MSG is trimmed beforehand."
  (let* ((pad (make-string offset ?\s))
         (rep (concat "\n" pad)))
    (concat pad
            (replace-regexp-in-string "\n" rep (string-trim msg)))))


;;; Customization

(defgroup flycheck-inline nil
  "Display Flycheck errors inline."
  :prefix "flycheck-inline-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-inline"))

(defface flycheck-inline-error
  '((t :inherit compilation-error))
  "Flycheck-inline face for errors."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defface flycheck-inline-warning
  '((t :inherit compilation-warning))
  "Flycheck-inline face for warnings."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defface flycheck-inline-info
  '((t :inherit compilation-info))
  "Flycheck-inline face for informational messages."
  :package-version '(flycheck-inline . "0.1")
  :group 'flycheck-inline)

(defcustom flycheck-inline-display-function #'flycheck-inline-display-phantom
  "Function to display inline errors.

This function is used to display inline all errors at point, as
well as all related errors.  It has the signature (MSG &optional
POS ERR), where MSG is the error message to display, POS its
buffer position, and ERR is the flycheck error in general."
  :group 'flycheck-inline
  :package-version '(flycheck-inline . "0.1")
  :type '(function :tag "Inline error display function")
  :risky t)

(defcustom flycheck-inline-clear-function #'flycheck-inline-clear-phantoms
  "Function to clear all inline errors.

It takes no arguments and should remove all inline errors created
by `flycheck-inline-display-function'."
  :group 'flycheck-inline
  :package-version '(flycheck-inline . "0.1")
  :type '(function :tag "Inline error clear function")
  :risky t)

(defcustom flycheck-inline-display-error-id t
  "Whether to display error IDs inline.

If non-nil, inline errors will contain the error ID.  Error IDs
are optional: not all checkers suplpy this information.  Error
IDs can also be seen in Flycheck's error list."
  :group 'flycheck-inline
  :type 'boolean
  :package-version '(flycheck-inline . "0.1")
  :safe #'booleanp)


;;; Displaying inline errors with phantoms

(defun flycheck-inline--displayed-p (err)
  "Whether the given error is displayed with any inline overlays."
  (seq-find (lambda (p) (eq err (overlay-get p 'error)))
            flycheck-inline--phantoms))

(defvar-local flycheck-inline--phantoms nil
  "Remember which phantoms were added to the buffer.")

(defun flycheck-inline-display-phantom (msg &optional pos err)
  "Display MSG at POS representing error ERR using phantoms.

POS defaults to point."
  (unless (flycheck-inline--displayed-p err)
    (push (flycheck-inline-phantom-display msg pos err) flycheck-inline--phantoms)))

(defun flycheck-inline-clear-phantoms ()
  "Remove all phantoms from buffer that don't contain point."
  (setq flycheck-inline--phantoms
        (seq-remove #'flycheck-inline-phantom-delete flycheck-inline--phantoms)))



;;; Display inline errors

(defun flycheck-inline--error-position (err)
  "Return the position to insert ERR at."
  (if (flycheck-relevant-error-other-file-p err)
      ;; Display overlays for other-file errors on the first line
      (point-min)
    (flycheck-error-pos err)))

(defun flycheck-inline--error-message (err)
  "Return the message to display for ERR."
  (let ((filename (flycheck-error-filename err))
        (id (flycheck-error-id err)))
    (concat (when (and filename (not (equal filename (buffer-file-name))))
              (format "In \"%s\":\n" (file-relative-name filename default-directory)))
            (flycheck-error-message err)
            (when (and id flycheck-inline-display-error-id)
              (format " [%s]" id)))))

(defun flycheck-inline--error-face (err)
  "Return the face used to display ERR."
  (pcase (flycheck-error-level err)
    (`info 'flycheck-inline-info)
    (`warning 'flycheck-inline-warning)
    (`error 'flycheck-inline-error)))

(defun flycheck-inline-display-error (err)
  "Display `flycheck-error' ERR inline."
  (let* ((pos (flycheck-inline--error-position err))
         (msg (propertize (flycheck-inline--error-message err)
                          'face (flycheck-inline--error-face err))))
    (funcall flycheck-inline-display-function msg pos err)))

(defun flycheck-inline-hide-errors ()
  "Hide all inline messages currently being shown."
  (funcall flycheck-inline-clear-function))

(defun flycheck-inline-display-errors (errors)
  "Display ERRORS, and all related errors, inline.

ERRORS is a list of `flycheck-error' objects."
  (flycheck-inline-hide-errors)
  (mapc #'flycheck-inline-display-error
        (seq-uniq
         (seq-mapcat #'flycheck-related-errors errors))))


;;; Global and local minor modes

;;;###autoload
(define-minor-mode flycheck-inline-mode
  "A minor mode to show Flycheck error messages line.

When called interactively, toggle `flycheck-inline-mode'.  With
prefix ARG, enable `flycheck-inline-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `flycheck-inline-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`flycheck-inline-mode'.  Otherwise behave as if called
interactively.

In `flycheck-inline-mode', show Flycheck error messages inline,
directly below the error reported location."
  :group 'flycheck-inline
  :require 'flycheck-inline
  (cond
   ;; Use our display function.
   (flycheck-inline-mode
    (setq-local flycheck-display-errors-function #'flycheck-inline-display-errors)
    (add-hook 'post-command-hook #'flycheck-inline-hide-errors nil 'local))
   ;; Reset the display function and remove ourselves from all hooks but only
   ;; if the mode is still active.
   ((not flycheck-inline-mode)
    (kill-local-variable 'flycheck-display-errors-function)
    (flycheck-inline-hide-errors)
    (remove-hook 'post-command-hook #'flycheck-inline-hide-errors 'local))))

(defun turn-on-flycheck-inline ()
  "Turn on `flycheck-inline-mode' in Flycheck buffers."
  ;; Make sure to turn on flycheck-inline in this buffer, either directly if
  ;; flycheck is already loaded, or via a hook if flycheck hasn't been loaded
  ;; yet.
  (if flycheck-mode
      (flycheck-inline-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode nil 'local)))

;;;###autoload
(define-global-minor-mode global-flycheck-inline-mode
  flycheck-inline-mode turn-on-flycheck-inline
  "Toggle flycheck-inline in all Flycheck buffers."
  :group 'flycheck-inline
  :require 'flycheck-inline)

(provide 'flycheck-inline)

;;; flycheck-inline.el ends here
