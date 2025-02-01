;;; sideline-flycheck.el --- Show flycheck errors with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Shen, Jen-Chieh
;; Created date 2022-06-14 17:10:48

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-flycheck
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (sideline "0.1.1") (flycheck "0.14") (ht "2.4"))
;; Keywords: convenience flycheck

;; This file is NOT part of GNU Emacs.

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
;;
;; This package allows display flycheck errors with sideline.
;;
;; 1) Add sideline-flycheck to sideline backends list,
;;
;;   (setq sideline-backends-right '(sideline-flycheck))
;;
;; 2) Then enable sideline-mode in the target buffer,
;;
;;   M-x sideline-mode
;;

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'sideline)
(require 'flycheck)
(require 'ht)

(defgroup sideline-flycheck nil
  "Show flycheck errors with sideline."
  :prefix "sideline-flycheck-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline-flycheck"))

(defcustom sideline-flycheck-display-mode 'point
  "Method type to when sideline will display flycheck's errors."
  :type '(choice (const line)
                 (const point))
  :group 'sideline-flycheck)

(defcustom sideline-flycheck-show-checker-name nil
  "If non-nil, show the checker's name at the back."
  :type 'boolean
  :group 'sideline-flycheck)

(defcustom sideline-flycheck-max-lines 1
  "Maximum number of lines to show."
  :type 'integer
  :group 'sideline-flycheck)

(defvar-local sideline-flycheck--callback nil
  "Callback to display errors with sideline.")

(defvar-local sideline-flycheck--timer nil
  "Timer to display diagnostics.")

(defvar-local sideline-flycheck--errors (ht-create)
  "Store error messages as key.")

(defface sideline-flycheck-error
  `((t :inherit error))
  "Indicate error operation."
  :group 'sideline-flycheck)

(defface sideline-flycheck-warning
  `((t :inherit warning))
  "Indicate warning operation."
  :group 'sideline-flycheck)

(defface sideline-flycheck-info
  `((t :inherit success))
  "Indicate info operation."
  :group 'sideline-flycheck)

(defcustom sideline-flycheck-error-prefix ""
  "Prefix for error sideline."
  :type 'string
  :group 'sideline-flycheck)

(defcustom sideline-flycheck-warning-prefix ""
  "Prefix for warning sideline."
  :type 'string
  :group 'sideline-flycheck)

(defcustom sideline-flycheck-info-prefix ""
  "Prefix for info sideline."
  :type 'string
  :group 'sideline-flycheck)

;;
;;; Core

;;;###autoload
(defun sideline-flycheck (command)
  "Backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async
                       (lambda (callback &rest _)
                         (setq sideline-flycheck--callback callback))))))

(defun sideline-flycheck--get-errors ()
  "Return flycheck errors."
  (cl-case sideline-flycheck-display-mode
    (`point (flycheck-overlay-errors-at (point)))
    (`line (flycheck-overlay-errors-in (line-beginning-position) (1+ (line-end-position))))
    (t (user-error "Invalid value of `sideline-flycheck-display-mode': %s"
                   sideline-flycheck-display-mode))))

;; XXX: Not sure if needed.
(defun sideline-flycheck--get-level (level)
  "Return level symbol by LEVEL."
  (cond ((string-match-p "warning" level) 'warning)
        ((string-match-p "error" level) 'error)
        (t 'info)))

(defun sideline-flycheck--show (&optional buffer)
  "Display ERRORS in BUFFER, using sideline library."
  (sideline--with-buffer-window (or buffer (current-buffer))
    (when-let ((sideline-mode)
               (errors (sideline-flycheck--get-errors))
               (sideline-flycheck--callback)  ; Make sure callback exists
               ;; XXX: Prevnet duplicate.
               ((not (sideline-backend-ovs 'sideline-flycheck))))
      (let (msgs)
        (dolist (err errors)
          (let* ((level (sideline-2str (flycheck-error-level err)))
                 ;; XXX: Safety guard; is this necessary?
                 (level (sideline-flycheck--get-level level))
                 (face (pcase level
                         (`warning 'sideline-flycheck-warning)
                         (`error   'sideline-flycheck-error)
                         (`info    'sideline-flycheck-info)))
                 (prefix (pcase level
                           (`warning sideline-flycheck-warning-prefix)
                           (`error   sideline-flycheck-error-prefix)
                           (`info    sideline-flycheck-info-prefix)))
                 (msg (flycheck-error-message err))
                 (lines (split-string msg "\n"))
                 (lines (butlast lines (- (length lines) sideline-flycheck-max-lines)))
                 (msg (mapconcat #'identity lines "\n"))
                 (msg (concat prefix msg))
                 (checker (flycheck-error-checker err)))
            (when sideline-flycheck-show-checker-name
              (setq msg (format "%s (%s)" msg checker)))
            (add-face-text-property 0 (length msg) face nil msg)
            (unless (ht-contains-p sideline-flycheck--errors msg)
              (ht-set sideline-flycheck--errors msg nil)  ; doesn't care about value
              (push msg msgs))))
        (funcall sideline-flycheck--callback msgs)))))

(defun sideline-flycheck--post-command ()
  "Display error message at point with a delay, unless already displayed."
  ;; XXX: Run only when the errors exist!
  ;;
  ;; We need this to prevent overlays being deleted from the function
  ;; `sideline-flycheck--after-check'. Another way to interpret this, is to
  ;; check if flycheck is currently doing the syntax check.
  (when (sideline-flycheck--get-errors)
    (when (timerp sideline-flycheck--timer) (cancel-timer sideline-flycheck--timer))
    (setq sideline-flycheck--timer
          (run-at-time (or flycheck-display-errors-delay
                           (default-value 'flycheck-display-errors-delay))
                       nil
                       #'sideline-flycheck--show (current-buffer)))))

(defun sideline-flycheck--reset ()
  "After sideline is reset."
  (ht-clear sideline-flycheck--errors))

(defun sideline-flycheck--after-check ()
  "Run after syntax check."
  (sideline-delete-backend-ovs 'sideline-flycheck)
  (sideline-flycheck--reset)
  (sideline-flycheck--show))

;;;###autoload
(defun sideline-flycheck-setup ()
  "Setup for `flycheck-mode'."
  (cond
   (flycheck-mode
    (add-hook 'flycheck-after-syntax-check-hook #'sideline-flycheck--after-check nil t)
    (add-hook 'post-command-hook #'sideline-flycheck--post-command nil t)
    (add-hook 'sideline-reset-hook #'sideline-flycheck--reset nil t))
   (t
    (remove-hook 'flycheck-after-syntax-check-hook #'sideline-flycheck--after-check t)
    (remove-hook 'post-command-hook #'sideline-flycheck--post-command t)
    (remove-hook 'sideline-reset-hook #'sideline-flycheck--reset t))))

(provide 'sideline-flycheck)
;;; sideline-flycheck.el ends here
