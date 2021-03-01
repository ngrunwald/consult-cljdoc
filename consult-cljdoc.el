;;; consult-cljdoc.el --- Interactively browse cljdoc.org -  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nils Grunwald <github.com/nilsgrunwald>
;; Author: Nils Grunwald
;; URL: http://github.com/nilsgrunwald/consult-cljdoc.el
;; Created: 2021
;; Version: 0.1.0
;; Keywords: clojure clojurescript cljdoc documentation
;; Package-Requires: ((request "20210214.37") (consult "20210301.1307"))

;; This file is NOT part of GNU Emacs.

;; consult-cljdoc.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; consult-cljdoc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an easy interactive interface to query cljdoc.org

;;; Code:
(require 'request)
(require 'consult)
(require 'seq)

(defgroup consult-cljdoc nil
  "Easy interactive queries to cljdoc.org."
  :prefix "consult-cljdoc-")

(defconst consult-cljdoc-version "0.1.0")

(defun consult-cljdoc--parse-item (item)
  (let ((group-id (cdr (assoc 'group-id item)))
        (artifact-id (cdr (assoc 'artifact-id item)))
        (version (cdr (assoc 'version item)))
        (score (cdr (assoc 'score item)))
        (description (cdr (assoc 'description item))))
    (propertize (format "%s/%s {:mvn/version \"%s\"}"
                        group-id
                        artifact-id
                        version)
                'group-id group-id
                'artifact-id artifact-id
                'version version
                'score score
                'description description
                'uri (format "https://cljdoc.org/d/%s/%s/%s" group-id artifact-id version))))

(defun consult-cljdoc--complete-request (next query)
  (when (and (s-present? query) (>= (length query) 3))
    (request (format "https://cljdoc.org/api/search?q=%s" query)
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall next 'flush)
                  (funcall next (seq-map (lambda (it) (consult-cljdoc--parse-item it))
                                         (cdr (assoc 'results data)))))))))

(defun consult-cljdoc--async-search (next)
  (lambda (action)
    (pcase action
      ((pred stringp)
       (consult-cljdoc--complete-request next action))
      (_ (funcall next action)))))

(defun consult-cljdoc--search-generator ()
  (thread-first (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult-cljdoc--async-search)
    (consult--async-throttle)
    (consult--async-split)))

(defun consult-cljdoc--consult-lookup (_input cands cand)
  (seq-find (lambda (x) (string= cand x)) cands))

(defun consult-cljdoc-marginalia-annotate (cand)
  (marginalia--fields
   ((get-text-property 0 'description cand) :face )))

(add-to-list 'marginalia-annotators-heavy
             '(cljdoc-artifact . consult-cljdoc-marginalia-annotate))

;;;###autoload
(defun consult-cljdoc ()
  "Interactively select an artifact in cljdoc and display its documentation page in browser."
  (interactive)
  (let* ((selected (consult--read (consult-cljdoc--search-generator)
                                  :prompt "Artifact Name: "
                                  :sort nil
                                  :category 'cljdoc-artifact
                                  :initial consult-async-default-split
                                  :require-match t
                                  :lookup 'consult-cljdoc--consult-lookup))
         (uri (get-text-property 0 'uri selected)))
    (browse-url uri)))

(provide 'consult-cljdoc)
;;; consult-cljdoc.el ends here
