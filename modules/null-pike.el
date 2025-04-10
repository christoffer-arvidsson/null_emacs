;;; null-pike.el --- pike configuration for null -*- lexical-binding: t -*-

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

(use-package pike
  :ensure nil  ; local
  :custom
  (pike-cache-directory (no-littering-expand-var-file-name "pike/"))
  :config
  (require 'tab-line)
  (setq tab-line-tabs-function #'pike-tab-line-tabs-function
        tab-line-close-button nil
        tab-line-new-button nil
        tab-line-separator " "
        tab-line-exclude-modes (list 'pike-mode))
  (global-tab-line-mode +1))

(general-define-key
 :states 'normal
 "M-N" #'pike-find-global-1
 "M-E" #'pike-find-global-2
 "M-I" #'pike-find-global-3
 "M-O" #'pike-find-global-4
 "M-n" #'pike-find-1
 "M-e" #'pike-find-2
 "M-i" #'pike-find-3
 "M-o" #'pike-find-4)

(null-keybinds-leader-key-def
  :states 'normal
  "' '" #'(lambda () (interactive) (pike-open-buffer nil))
  "' a" #'(lambda () (interactive) (pike-add-key nil))
  "' X" #'(lambda () (interactive) (pike-clear nil))
  "' x" #'pike-delete-current
  "' p" #'pike-promote-current
  "' d" #'pike-demote-current
  "` `" #'(lambda () (interactive) (pike-open-buffer t))
  "` a" #'(lambda () (interactive) (pike-add-key t))
  "` X" #'(lambda () (interactive) (pike-clear t)))

(provide 'null-pike)

;;; null-pike.el ends here
