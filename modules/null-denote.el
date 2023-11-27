;;; null-denote.el --- Denote module -*- lexical-binding: t -*-

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

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/notes")
  (denote-known-keywords '("work")))

(null-keybinds-leader-key-def
  :keymaps 'global-map
  "n w" '(:ignore t :wk "Denote")
  "n w f" 'denote-open-or-create
  "n w R" 'denote-rename-file
  "n w c" 'denote-create-note)

(provide 'null-denote)

;;; null-denote.el ends here
