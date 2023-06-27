;;; null-apps.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(require 'null-keybinds)

(use-package calc
  :ensure t)

(use-package vterm
  :ensure t)

(use-package fish-mode
  :ensure t)

(defun null/open-ranger-in-project-root ()
  "Open Ranger in the root directory of the current project."
  (interactive)
  (let* ((project (project-current))
         (project-root (and project (project-root project))))
    (if project-root
        (ranger project-root)
      (message "No project found."))))

(use-package ranger
  :ensure t)

;; Non-package keys
(null-keybinds-leader-key-def
  :states 'normal
  "." '(dired :wk "Open dired")
  "o c" '(quick-calc :wk "Quick calculator")
  "o t" '(vterm :wk "Open vterm")
  "o r" '(null/open-ranger-in-project-root :wk "Open ranger")
  "o C" '(calc :wk "Calculator"))


(provide 'null-apps)

;;; null-apps.el ends here
