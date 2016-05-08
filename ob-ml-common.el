;;; ob-ml-common.el --- common functions for ob-ml-* modes

;; Copyright (C) 2016 Norman Walsh

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

;; This file is a common library module.  See ob-ml-marklogic.el.

;;; Code:

(require 'ob)

(defvar ob-ml-common-default-header-args
  '((:ml-curl . "/usr/bin/curl")
    (:ml-host . "localhost")
    (:ml-scheme . "http")
    (:ml-port . 8000)
    (:ml-eval-path . "/v1/eval")
    (:ml-graphs-path . "/v1/graphs/sparql")
    (:ml-username . "admin")
    (:ml-password . "admin")
    (:ml-auth . "--digest")
    (:ml-output . "*ob-ml-marklogic output*")
    (:ml-save-output . nil)))

(defun ob-ml-common-execute (body params &optional default-language)
  "Execute the query in BODY using the specified PARAMS.
If no `:language' is specified in the block, DEFAULT-LANGUAGE is assumed.
The code is executed by passing it to MarkLogic for evaluation."
  (let* ((lparam   (cdr (assq :language params)))
         (language (cond ((eq nil lparam) default-language)
                         ((string= "xquery" lparam) "xquery")
                         ((string= "xqy" lparam) "xquery")
                         ((string= "javascript" lparam) "javascript")
                         ((string= "js" lparam) "javascript")
                         ((string= "sjs" lparam) "javascript")
                         ((string= "sparql" lparam) "sparql")
                         (t (error (concat "Unexpected language: " lparam)))))
         (qname    (if (string= "sparql" language)
                       "query"
                     language))
         (uripfx   (concat (cdr (assq :ml-scheme params))
                           "://"
                           (cdr (assq :ml-host params))
                           ":"
                           (number-to-string (cdr (assq :ml-port params)))))
         (path     (if (string= "sparql" language)
                       (cdr (assq :ml-graphs-path params))
                     (cdr (assq :ml-eval-path params))))
         (process  nil)
         (sep      "?")
         (uvar     nil)
         (qvar     nil)
         (bufname  (cdr (assq :ml-output params)))
         (tempbuf (get-buffer-create bufname))
         (results ""))
    (save-current-buffer
      (dolist (param params)
        (if (eq :var (car param))
            (let ((name (symbol-name (car (cdr param))))
                  (tname (substring (symbol-name (car (cdr param))) 1))
                  (value (if (numberp (cddr param))
                             (number-to-string (cddr param))
                           (cddr param))))
              (if (equal "&" (substring name 0 1))
                  (if (or (equal "database" tname)
                          (equal "txid" tname))
                      (progn
                        (setq path (concat path sep tname "=" value))
                        (setq sep "&"))
                    (setq uvar (cons "-d" (cons (concat tname "=" value) uvar))))
                (setq qvar (cons (concat "\"" name "\":\"" value "\"") qvar))))))
      (if (not (eq nil qvar))
          (setq uvar (cons "-d"
                           (cons
                            (concat "vars={" (mapconcat 'identity qvar ",") "}")
                            uvar))))
      (setq process
            (if (cdr (assq :ml-auth params))
                (append
                 (list 'call-process (cdr (assq :ml-curl params)) nil bufname nil
                       "-v" "-s" "-X" "POST" (cdr (assq :ml-auth params))
                       "-u" (concat (cdr (assq :ml-username params))
                                    ":" (cdr (assq :ml-password params)))
                       "-d" (concat qname "=" body))
                 uvar (list (concat uripfx path)))
              (append
               (list 'call-process (cdr (assq :ml-curl params)) nil bufname nil
                     "-v" "-s" "-X" "POST" "-d" (concat qname "=" body))
               uvar (list (concat uripfx path)))))
      (eval process)
      (set-buffer tempbuf)
      (setq results (ob-ml-common--get-results))
      (if (not (and (assq :ml-save-output params) (cdr (assq :ml-save-output params))))
          (kill-buffer bufname))
      results)))

(defun ob-ml-common--get-results ()
  "Parse the output of curl, extracting the results."
  (let ((line nil)
        (lastline nil)
        (count 0)
        (headers nil))
    (save-current-buffer
      (save-match-data
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (if (search-forward "upload completely sent off")
            (progn
              (beginning-of-line)
              (forward-line)
              (delete-region 1 (point))
              (forward-line)
              (setq headers (ob-ml-common--get-headers "< "))
              (delete-region 1 (point))
              (re-search-forward "\\* Connection .* to host .* left intact")
              (beginning-of-line)
              (forward-line)
              (delete-region 1 (point))))
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
              (beginning-of-line)
              (forward-line)
              (setq headers (ob-ml-common--get-headers nil headers))
              (forward-line)
              (delete-region 1 (point))
              (re-search-forward lastline nil t)
              (beginning-of-line)
              (backward-char 1)
              (setq spos (point))
              (goto-char (point-max))
              (delete-region spos (point))))

        (if (< count 2)
            (let ((ctype (cdr (assoc "content-type" headers))))
              (cond ((eq ctype nil) nil)
                    ((and (or (string-match "application/json" ctype)
                              (string-match "application/.*\\+json" ctype))
                          (fboundp 'json-reformat-region))
                     (progn
                       (goto-char (point-max))
                       (json-reformat-region 1 (point))))
                    ((and (or (string-match "application/xml" ctype)
                              (string-match "text/xml" ctype)
                              (string-match "application/.*\\+xml" ctype))
                          (featurep 'nxml-mode))
                     (progn
                       (goto-char (point-max))
                       (ob-ml-common--xml-reformat-region 1 (point))))
                    (t nil))))
        (buffer-string)))))

(defun ob-ml-common--get-headers (&optional pfx headers)
  "Parse MIME headers from the buffer.  Crudely.
This function looks for lines that appear to be headers (`Name: value' pairs).
It advances forward through the buffer recording the headers that it finds
until it encounters a line that doesn't appear to be a header.

The code for identifying headers is very crude.  I didn't look for a proper
regex for header names and it doesn't handle multi-line headers.  That
seems to be fine for this application.

If PFX is specified, the specified prefix must occur before the
header name on each line.  If a list of HEADERS is
provided (presumably from an earlier call to this function), it
will be updated and the updated result returned."
  (let ((pattern (if (eq nil pfx)
                     "^\\([-a-zA-Z0-9]+\\): \\(.*\\)$"
                   (concat "^" pfx "\\([-a-zA-Z0-9]+\\): \\(.*\\)$"))))
    (while (looking-at pattern)
      (let* ((key (downcase (match-string-no-properties 1)))
             (value (match-string-no-properties 2))
             (cell (assoc key headers)))
        (if cell
            (setcdr cell value)
          (add-to-list 'headers (cons key value)))
        (forward-line)))
    headers))

;; https://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun ob-ml-common--xml-reformat-region (begin end)
  "Use a combination of regex hacking and `nxml-mode' to reformat XML.
The region between BEGIN and END will be reformatted.  Any
whitespace-only nodes between elements are considered fair game
for reformatting.  Copied from
https://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs"
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(provide 'ob-ml-common)

;;; ob-ml-common.el ends here
