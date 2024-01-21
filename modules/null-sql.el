;;; null-sql.el --- SQL configuration module -*- lexical-binding: t -*-

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

;; Nothing as of yet

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;; Code:
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlup-mode
  :hook
  (sql-mode . sqlup-mode)
  (sql-interactive-mode . sqlup-mode))

(null-keybinds-major-key-def
  :states '(normal visual)
  :keymaps 'sql-mode-map
  "r" '(:ignore t :wk "run")
  "r s" 'sql-sqlite
  "r p" 'sql-postgres

  "e" '(:ignore t :wk "eval")
  "e b" 'sql-send-buffer
  "e r" 'sql-send-region
  "e p" 'sql-send-paragraph)

(provide 'null-sql)

;;; null-sql.el ends here
