;;; helm-find-files-in-project.el --- Find files in project from helm

;; Copyright (C) 2012-2013  Toshiyuki Takahashi

;; Author: Toshiyuki Takahashi (@tototoshi)
;; Keywords:

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

;;; Usage:
;;;
;;; (require 'helm-find-files-in-project)
;;;
;;; Code:

;;; find files in current project

(defvar hffip:filter-pattern
  "\\~\\|\\.git\\|target/\\|\\.class\\|\\.svn")

(defun hffip:dirname (file)
  (chomp (shell-command-to-string (format "dirname %s" file))))

(defun hffip:find-project-root ()
  (expand-file-name
   (or (locate-dominating-file default-directory ".git")
       (locate-dominating-file default-directory "pom.xml")
       (locate-dominating-file default-directory "build.sbt")
       (locate-dominating-file default-directory "Gemfile")
       (locate-dominating-file default-directory "setup.py"))))

(defun hffip:remove-trailing-backslash (s)
  (replace-regexp-in-string "/$" "" s))

(defun hffip:abspath-to-relative-path (abspath)
  (replace-regexp-in-string (hffip:find-project-root) "" s))

(defun helm-c-source-files-under-tree-candidates-function ()
  (let ((project-root (hffip:find-project-root)))
    (when project-root
      (mapcar
       '(lambda (s) (hffip:abspath-to-relative-path s))
       (split-string
        (shell-command-to-string
         (print (format "find %s -type f | grep -v '%s'"
                        (hffip:remove-trailing-backslash project-root)
                        hffip:filter-pattern)))
        "\n")))))

(defvar helm-c-source-files-in-project
  '((name . "Files in project")
    (candidates . helm-c-source-files-under-tree-candidates-function)
    (action . find-file)))

(defun helm-find-files-in-project ()
  (interactive)
  (helm 'helm-c-source-files-in-project))

(provide 'helm-find-files-in-project)

;;; helm-find-files-in-project.el ends here

