;;; helm-find-files-in-project.el --- Find files in project from helm

;; Copyright (C) 2012-2014  Toshiyuki Takahashi

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
   (or (locate-dominating-file current-directory ".git")
       (locate-dominating-file current-directory "pom.xml")
       (locate-dominating-file current-directory "build.sbt")
       (locate-dominating-file current-directory "Gemfile")
       (locate-dominating-file current-directory "setup.py"))))

(defun hffip:remove-trailing-backslash (s)
  (replace-regexp-in-string "/$" "" s))

(defun hffip:abspath-to-relative-path (abspath)
  (replace-regexp-in-string (hffip:find-project-root) "" s))

(defun helm-c-source-files-under-tree-candidates-function (buf)
  (let ((project-root (hffip:find-project-root)))
    (when project-root
      (shell-command
        (format "find %s | grep -v '%s'" (hffip:remove-trailing-backslash project-root) hffip:filter-pattern)
        buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (replace-string project-root "")))))

(defun hffip:find-file (file)
  (find-file (concat (hffip:find-project-root) file)))

(setq helm-c-source-files-in-project
  '((name . "Files in project")
    (init . (lambda ()
              (helm-c-source-files-under-tree-candidates-function (helm-candidate-buffer 'global))))
    (candidates-in-buffer)
    (action . hffip:find-file)))

(defun helm-find-files-in-project ()
  (interactive)
  (let ((current-directory default-directory))
    (helm 'helm-c-source-files-in-project)))

(provide 'helm-find-files-in-project)

;;; helm-find-files-in-project.el ends here
