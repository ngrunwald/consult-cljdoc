;;; consult-cljdoc.el --- Interactively browse cljdoc.org. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/consult-cljdoc
;; Created: 2021
;; Version: 0.1.0
;; Keywords: clojure, clojurescript, cljdoc, documentation
;; Package-Requires: ((request "20210214.37") (consult "20210301.1307") (parseedn "20200419.1124"))

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
;; along with consult-cljdoc.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an easy interactive interface to query cljdoc.org

;;; Code:
(require 'request)
(require 'consult)
(require 'seq)
(require 's)
(require 'parseedn)
(require 'subr-x)
(require 'cl-macs)
(require 'marginalia)

(defgroup consult-cljdoc nil
  "Easy interactive queries to cljdoc.org."
  :prefix "consult-cljdoc-")

(defconst consult-cljdoc-version "0.1.0")

(defvar consult-cljdoc-history nil)

(defun consult-cljdoc--parse-item (item)
  (let ((group-id (cdr (assoc 'group-id item)))
        (artifact-id (cdr (assoc 'artifact-id item)))
        (version (cdr (assoc 'version item)))
        (score (cdr (assoc 'score item)))
        (category (cdr (assoc 'category item)))
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
                'category category
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
   ((get-text-property 0 'description cand))))

(add-to-list 'marginalia-annotators-heavy
             '(cljdoc-artifact . consult-cljdoc-marginalia-annotate))

(defun consult-cljdoc--parse-artifact (artifact)
  (let ((parsed (s-split "/" (symbol-name artifact) t)))
    `(,(first parsed) ,(first (last parsed 1)))))

(defun consult-cljdoc--parse-deps-map (deps category)
  (let ((results '()))
    (maphash (lambda (k v)
               (let* ((parsed-art (consult-cljdoc--parse-artifact k))
                      (group-id (first parsed-art))
                      (artifact-id (second parsed-art))
                      (version (gethash :mvn/version v)))
                 (when (and group-id artifact-id version)
                   (push `((group-id    . ,group-id)
                           (artifact-id . ,artifact-id)
                           (version     . ,version)
                           (category    . ,category))
                         results))))
             deps)
    results))

(defun consult-cljdoc--keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun consult-cljdoc--parse-deps-edn-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((content (car (parseedn-read)))
           (deps (gethash :deps content))
           (cands (consult-cljdoc--parse-deps-map deps :main))
           (aliases (gethash :aliases content)))
      (maphash (lambda (k v)
                 (let ((deps (consult-cljdoc--parse-deps-map (or (gethash :extra-deps v)
                                                                 (gethash :replace-deps v)
                                                                 (make-hash-table))
                                                             k)))
                   (seq-each (lambda (it) (push it cands)) deps)))
               aliases)
      (seq-map #'consult-cljdoc--parse-item (seq-uniq cands)))))

(defun consult-cljdoc--parse-lein-deps (category deps)
  (seq-map
   (lambda (dep)
     (let* ((coord (elt dep 0))
            (version (elt dep 1))
            (parsed-art (consult-cljdoc--parse-artifact coord))
            (group-id (first parsed-art))
            (artifact-id (second parsed-art)))
       (propertize (format "[%s \"%s\"]"
                           (if (string-equal group-id artifact-id)
                               group-id
                             (format "%s/%s" group-id artifact-id))
                           version)
                   'group-id group-id
                   'artifact-id artifact-id
                   'version version
                   'category category
                   'uri (format "https://cljdoc.org/d/%s/%s/%s" group-id artifact-id version))))
   deps))

(defun consult-cljdoc--parse-project-clj-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((content (car (parseedn-read)))
           (raw-data (seq-drop content 3))
           (data (make-hash-table)))
      (seq-each
       (lambda (pair) (puthash (first pair) (second pair) data))
       (seq-partition raw-data 2))
      (let* ((main-deps `(:main . ,(gethash :dependencies data)))
             (profiles (gethash :profiles data))
             (all-deps (list main-deps)))
        (maphash (lambda (k v)
                   (let ((deps (gethash :dependencies v)))
                     (push `(,k . ,deps) all-deps)))
                 profiles)
        (seq-mapcat (lambda (g) (consult-cljdoc--parse-lein-deps (car g) (cdr g)))
                    (reverse all-deps))))))

(defun consult-cljdoc--extract-category (cand transform)
  (if transform
      cand
    (get-text-property 0 'category cand)))

(defun consult-cljdoc--find-project-config-file ()
  (let ((deps-dir (locate-dominating-file default-directory "deps.edn")))
    (if deps-dir
        `(:deps . ,(s-concat deps-dir "deps.edn"))
      (let ((lein-dir (locate-dominating-file default-directory "project.clj")))
        (when lein-dir
          `(:lein . ,(s-concat deps-dir "project.clj")))))))

;;;###autoload
(defun consult-cljdoc-browse-project-documentation ()
  (interactive)
  (let* ((res (consult-cljdoc--find-project-config-file))
         (cands (pcase (car res)
                  (:deps (consult-cljdoc--parse-deps-edn-file (cdr res)))
                  (:lein (consult-cljdoc--parse-project-clj-file (cdr res))))))
    (if cands
        (let* ((selected (consult--read cands
                                        :prompt "Artifact Name: "
                                        :require-match t
                                        :category 'cljdoc-artifact
                                        :lookup 'consult-cljdoc--consult-lookup
                                        :group 'consult-cljdoc--extract-category
                                        ))
               (full (seq-find (lambda (x) (string= x selected)) cands)))
          (browse-url (get-text-property 0 'uri full)))
      (message "No project file found... Are you even in a clojure project, bro?"))))

;;;###autoload
(defun consult-cljdoc ()
  "Interactively select an artifact in cljdoc and display its documentation page in browser."
  (interactive)
  (let* ((selected (consult--read (consult-cljdoc--search-generator)
                                  :prompt "Artifact Name: "
                                  :sort nil
                                  :category 'cljdoc-artifact
                                  :initial "#"
                                  :require-match t
                                  :history 'consult-cljdoc-history
                                  :lookup 'consult-cljdoc--consult-lookup))
         (uri (get-text-property 0 'uri selected)))
    (browse-url uri)))

(provide 'consult-cljdoc)
;;; consult-cljdoc.el ends here
