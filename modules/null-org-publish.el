;;; null-org-publish.el --- Org site publishing functinoality -*- lexical-binding: t -*-

;; Author: Christoffer Arvidsson
;; Maintainer: Christoffer Arvidsson
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Publishing my org roam files.

;;; Code:

(use-package ox-hugo
   :ensure t)

(defun orbit/join-dirs (root &rest dirs)
  "Joins a series of directories DIRS together starting at ROOT."
  (if (not dirs)
      root
    (apply 'orbit/join-dirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defvar orbit/directory "~/Dropbox/org/orbit"
  "The Orbit base directory.")

(defvar orbit/public-directory (orbit/join-dirs orbit/directory "public")
  "The directory to publish Orbit to.")

(defun orbit/serve ()
  "Start Orbit webserver."
  (interactive)
  (httpd-serve-directory orbit/public-directory)
  (browse-url "127.0.0.1:8080/articles/sitemap.html"))

(null-keybinds-leader-key-def
  :states '(normal motion visual emacs)
  "n o" '(:ignore t :wk "orbit")
  "n o o" '(orbit/serve :wk "Start orbit"))

(provide 'null-org-publish)

;;; null-org-publish.el ends here
