;;; null-snippets-yas.el --- Snippet support with yasnippet -*- lexical-binding: t -*-

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

;;; Code:

(require 'null-keybinds)

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "misc/snippets-yasnippet/" user-emacs-directory)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure (:host github :repo "christoffer-arvidsson/yasnippet-snippets"))

(use-package yasnippet-radical-snippets
  :ensure (:host github :repo "Xaldew/yasnippet-radical-snippets" :files (:defaults "snippets" "yasnippet-radical-snippets.el"))
  :after yasnippet
  :config
  (yasnippet-radical-snippets-initialize))

(use-package consult-yasnippet
  :after consult yasnippet)

(use-package yasnippet-capf
  :after cape yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(null-keybinds-leader-key-def
  :states 'normal
  "i s" '(consult-yasnippet :wk "yasnippet insert snippet")
  "i S" '(consult-et-visit-snippet-file :wk "yasnippet visit snippet"))

(provide 'null-snippets-yas)

;;; null-snippets-yas.el ends here
