;;; ob-marklogic.el --- org-babel functions for MarkLogic evaluation

;; Copyright (C) 2016 Norman Walsh

;; Author: Norman Walsh
;; Keywords: MarkLogic, XQuery, JavaScript, SPARQL
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

;; This package provides org-mode support for evaluating code blocks
;; by sending them off to MarkLogic server.

;; XQuery, JavaScript and SPARQL are supported.  I've only tested on
;; MarkLogic 8.x or later.  YMMV on earlier releases.

;; There's nothing terribly fancy going on under the covers, this code
;; marshals the arguments to curl.  So you need to have a working
;; install of curl.  I've only tested this on Linux, but I expect it'll
;; work on Mac.  Not so sure about Windows.

;; Most of the configuration is done with header arguments.  These can
;; be specified at any level.  The following header arguments are
;; supported:

;; :ml-curl        The curl executable
;; :ml-host        The MarkLogic hostname
;; :ml-scheme      The URI scheme for requests
;; :ml-port        The port for requests
;; :ml-eval-path   The eval path
;; :ml-graphs-path The SPARQL eval path
;; :ml-auth        Type of auth
;; :ml-username    Username
;; :ml-password    Password
;; :ml-output      Output buffer
;; :ml-save-output Keep output buffer?

;; See the org-babel-default-header-args below for defaults.  You'll
;; probably need to change some of them.  The request URI is constructed
;; by concatenation:

;;     :ml-scheme "://" :ml-host ":" :ml-port :ml-*-path

;; If you don't specify :ml-auth, then the requests will be made without
;; authentication.  Setting :ml-save-output will prevent the temporary
;; buffer that's used to hold results from being deleted.  That can be
;; useful if something goes wrong.

;; You can also specify variables to the query, using the standard :var
;; header argument.  Variable names that start with "&" are passed
;; *to the eval endpoint*.  All other variable names are passed through
;; to the underlying query.

;; For example:

;;    #+begin_src marklogic :var startDate="2017-04-19T12:34:57"

;; This passes the variable "startDate" to the query (where it can
;; be accessed by declaring it external). Alternatively:

;;    #+begin_src marklogic :var &database="Documents"

;; This sets the "database" query parameter to the eval endpoint.
;; (We're careful to set "database" and "txid" parameters on the
;; URI so that they're accessible to the declarative rewriter; if
;; you don't know what that means, just ignore this parenthetical
;; comment.)

;; You can specify as many variables as you wish.  You'll no doubt get
;; errors if you pass things that the endpoint or query aren't expecting.
;; I have no idea how well my code plays with advanced org-mode features
;; like reference to other named code blocks.  If you see something
;; weird, please open an issue.

;; The results are very dependent on the "-v" output from curl.  Here's
;; what I expect:

;;    *   Trying 172.17.0.2...
;;      ...
;;    * upload completely sent off: 192 out of 192 bytes
;;    < HTTP/1.1 200 OK
;;    < Content-type: application/sparql-results+json; charset=UTF-8
;;    < Server: MarkLogic
;;    < Content-Length: 123
;;    < Connection: Keep-Alive
;;    < Keep-Alive: timeout=5
;;    <
;;    { [123 bytes data]
;;    * Connection #0 to host f23-builder left intact
;;
;;    ACTUAL RESULTS GO HERE

;; In brief: ignore all of the results up to the line that contains
;; "upload completely sent off".  Then skip the HTTP/1.1 and parse
;; the headers.  Then skip to the results.

;; If the response is multipart *and* there's only one part, the
;; multipart scaffolding is stripped away, taking care to parse the
;; part headers to get the actual content type.

;; Because...

;; If there's only one part:
;;
;;   * If it's JSON and 'json-reformat-region is available, the
;;     result is reformatted before returning it.
;;   * If it's XML and nxml-mode is available, the result
;;     is reformatted before returning it.
;;
;; If there's more than one part, you just get the whole thing as
;; it appeared on the wire.

;; TODO:
;;
;; * Consider reformatting the individual parts of a multipart
;;   response

;;; Code:

(require 'ob)

(defvar org-babel-default-header-args:marklogic
  '((:ml-curl . "/usr/bin/curl")
    (:ml-host . "f23-builder")
    (:ml-scheme . "http")
    (:ml-port . 8000)
    (:ml-eval-path . "/v1/eval")
    (:ml-graphs-path . "/v1/graphs/sparql")
    (:ml-username . "admin")
    (:ml-password . "admin")
    (:ml-auth . "--digest")
    (:ml-output . "*ob-marklogic output*")
    (:ml-save-output . nil)))

(defun org-babel-execute:marklogic (body params)
  "Execute the query in BODY using the specified PARAMS.
The code is executed by passing it to MarkLogic for evaluation.
This function is called by `org-babel-execute-src-block'."
  (let* ((lparam   (cdr (assq :language params)))
         (language (cond ((eq nil lparam) "xquery")
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
      (setq results (ob-marklogic-get-results))
      (if (not (and (assq :ml-save-output params) (cdr (assq :ml-save-output params))))
          (kill-buffer bufname))
      results)))

(defun org-babel-prep-session:marklogic (session params)
  "Raise an error if a SESSION is passed with PARAMS.
I haven't a clue what sessions are at the moment."
  (error "MarkLogic sessions are not supported at this time"))

(defun ob-marklogic-get-results ()
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
              (setq headers (ob-marklogic-get-headers "< "))
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
              (setq headers (ob-marklogic-get-headers nil headers))
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
                       (ob-marklogic-xml-reformat-region 1 (point))))
                    (t nil))))
        (buffer-string)))))

(defun ob-marklogic-get-headers (&optional pfx headers)
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
(defun ob-marklogic-xml-reformat-region (begin end)
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

(provide 'ob-marklogic)

;;; ob-marklogic.el ends here
