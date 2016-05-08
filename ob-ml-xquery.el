;;; ob-ml-xquery.el --- org-babel functions for MarkLogic XQuery evaluation

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

;; This file provides XQUERY support.  See ob-ml-marklogic.el.

;;; Code:

(require 'ob)
(require 'ob-ml-common)

(defvar org-babel-default-header-args:ml-xquery
  ob-ml-common-default-header-args)

(defun org-babel-execute:ml-xquery (body params)
  "Execute the query in BODY using the specified PARAMS.
The code is executed by passing it to MarkLogic for evaluation.
This function is called by `org-babel-execute-src-block'."
  (ob-ml-common-execute body params "xquery"))

(defun org-babel-prep-session:ml-xquery (session params)
  "Raise an error if a SESSION is passed with PARAMS.
I haven't a clue what sessions are at the moment."
  (error "MarkLogic sessions are not supported at this time"))

(provide 'ob-ml-xquery)

;;; ob-ml-xquery.el ends here
