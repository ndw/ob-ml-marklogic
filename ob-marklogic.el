;;; ob-marklogic.el --- org-babel functions for MarkLogic evaluation

;; Copyright (C) 2016 Norman Walsh

;; Author: Norman Walsh
;; Keywords: MarkLogic, XQuery, JavaScript
;; Homepage: http://github.com/ndw/ob-marklogic

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Doesn't support SPARQL (yet).

;;; Code:

(require 'ob)

(defvar org-babel-default-header-args:marklogic '())

(defvar ob-marklogic-curl "/usr/bin/curl")
(defvar ob-marklogic-eval "http://localhost:8000/v1/eval")
(defvar ob-marklogic-username "admin")
(defvar ob-marklogic-password "admin")
(defvar ob-marklogic-auth "--digest")
(defvar ob-marklogic-output "ob-marklogic output")

(defun org-babel-execute:marklogic (body params)
  "Execute a block of code by passing it to MarkLogic for evaluation.
This function is called by `org-babel-execute-src-block'."
  (let* ((lparam (cdr (assoc :language params)))
         (language (cond ((eq nil lparam) "xquery")
                         ((string= "xquery" lparam) "xquery")
                         ((string= "xqy" lparam) "xquery")
                         ((string= "javascript" lparam) "javascript")
                         ((string= "js" lparam) "javascript")
                         ((string= "sjs" lparam) "javascript")
                         (t (error (concat "Unexpected language: " lparam)))))
         (tempbuf (get-buffer-create ob-marklogic-output))
         (results ""))
    (progn
      (save-current-buffer
        (set-buffer ob-marklogic-output)
        (erase-buffer)
        (if ob-marklogic-auth
            (call-process ob-marklogic-curl nil ob-marklogic-output nil
                          "-s" "-X" "POST" ob-marklogic-auth
                          "-u" (concat ob-marklogic-username ":" ob-marklogic-password)
                          "-d" (concat language "=" body)
                          ob-marklogic-eval)
          (call-process ob-marklogic-curl nil ob-marklogic-output nil
                        "-s" "-X" "POST" "-d" (concat language "=" body)
                        ob-marklogic-eval)))
      (setq results (ob-marklogic-get-results))
      (kill-buffer ob-marklogic-output)
      results)))

(defun org-babel-prep-session:marklogic (session params)
  "Return an error if the :session header argument is set.
I haven't a clue what sessions are at the moment."
  (error "MarkLogic sessions are not supported at this time"))

(defun ob-marklogic-get-results ()
  (let ((line nil)
        (count 0))
    (save-current-buffer
      (set-buffer ob-marklogic-output)
      (delete-trailing-whitespace)
      (goto-char 2)
      (setq line (thing-at-point 'line t))
      (setq line (substring line 0 (- (length line) 1)))
      (setq lastline (concat "^" (regexp-quote line) "--$"))
      (setq line (concat "^" (regexp-quote line) "$"))
      (while (re-search-forward line nil t)
        (setq count (+ 1 count)))
      (if (= 1 count)
          (let ((spos 0)
                (epos 0))
            (goto-char 2)
            (re-search-forward line nil t)
            (search-forward "\C-j\C-j")
            (setq spos (point))
            (re-search-forward lastline nil t)
            (beginning-of-line)
            (backward-char 1)
            (setq epos (point))
            (buffer-substring spos epos))
        (buffer-string)))))

(provide 'ob-marklogic)

;;; ob-marklogic.el ends here
