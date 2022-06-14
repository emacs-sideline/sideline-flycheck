;;; sideline-flycheck.el --- Show flycheck errors with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-06-14 17:10:48

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Show flycheck errors with sideline
;; Keyword: sideline flycheck
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flycheck "0.14") (sideline "0.1.0"))
;; URL: https://github.com/jcs-elpa/sideline-flycheck

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
;; Show flycheck errors with sideline.
;;

;;; Code:

(require 'cl-lib)

(require 'flycheck)
(require 'sideline)

(defgroup sideline-flycheck nil
  "Show flycheck errors with sideline."
  :prefix "sideline-flycheck-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/sideline-flycheck"))

(defcustom sideline-flycheck-inhibit-functions nil
  "Functions to inhibit display of sideline flycheck."
  :type 'hook
  :group 'sideline-flycheck)

(defvar-local sideline-flycheck--old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

(defvar-local sideline-flycheck--callback nil
  "Callback to display errors with sideline.")

(defun sideline-flycheck--get-candidates (callback &rest _)
  "Asynchronous get flycheck errors."
  (setq sideline-flycheck--callback callback))

(defun sideline-flycheck--show (errors)
  "Display ERRORS, using sideline.el library."
  (when (and errors
             (not (run-hook-with-args-until-success 'sideline-flycheck-inhibit-functions))
             sideline-flycheck--callback)
    (funcall sideline-flycheck--callback (mapcar #'flycheck-error-message errors))))

;;;###autoload
(defun sideline-flycheck (command)
  "Backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async #'sideline-flycheck--get-candidates))))

;;;###autoload
(define-minor-mode sideline-flycheck-mode
  "A minor mode to show Flycheck error messages in a sideline."
  :lighter nil
  :group 'sideline-flycheck
  (cond
   ;; Use our display function and remember the old one but only if we haven't
   ;; yet configured it, to avoid activating twice.
   ((and sideline-flycheck-mode
         (not (eq flycheck-display-errors-function #'sideline-flycheck--show)))
    (setq sideline-flycheck--old-display-function flycheck-display-errors-function)
    (setq-local flycheck-display-errors-function #'sideline-flycheck--show))
   ;; Reset the display function and remove ourselves from all hooks but only
   ;; if the mode is still active.
   ((and (not sideline-flycheck-mode)
         (eq flycheck-display-errors-function #'sideline-flycheck--show))
    (setq-local flycheck-display-errors-function sideline-flycheck--old-display-function)
    (setq sideline-flycheck--old-display-function nil))))

(provide 'sideline-flycheck)
;;; sideline-flycheck.el ends here
