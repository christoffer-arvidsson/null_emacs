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
(require 'pike)

(setq pike-cache-directory (no-littering-expand-var-file-name "pike/"))

(general-define-key
 :states 'normal
 "M-N" #'pike-find-1
 "M-E" #'pike-find-2
 "M-I" #'pike-find-3
 "M-O" #'pike-find-4)

(null-keybinds-leader-key-def
  :states 'normal
  "` a" 'pike-add-key
  "` X" 'pike-clear
  "` `" 'pike-open-buffer)

(provide 'null-pike)

;;; null-pike.el ends here
