;;; null-rss.el --- summary -*- lexical-binding: t -*-

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

;; Just elfeed stuff for now!

;;; Code:

(require 'null-keybinds)
(require 'null-org)

(defcustom null/elfeed-directory (expand-file-name "elfeed" null/org-directory)
  "Path to elfeed directory")

(use-package elfeed
  :config
  (defun elfeed-display-buffer (buf &optional act)
      (pop-to-buffer buf)
      (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))
  :custom
  (elfeed-db-directory (expand-file-name "db/" null/elfeed-directory))
  (elfeed-show-entry-switch #'elfeed-display-buffer))

(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" null/elfeed-directory)))
  :config
  (elfeed-org))

;; Non-package keys
(null-keybinds-leader-key-def
  :states 'normal
  "o f" '(elfeed :wk "Open elfeed"))

(provide 'null-rss)

;;; null-rss.el ends here
