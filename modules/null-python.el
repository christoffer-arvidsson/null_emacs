;;; null-python.el --- summary -*- lexical-binding: t -*-

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

(use-package python
  :defer t
  :custom
  (python-indent-offset 4)
  :config
  (setenv "PYTHONPATH" "/home/s0001520/repos/madame-web")

  ;; Make evil treat words_words as one symbol
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w"))))

(use-package anaconda-mode
  :defer t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'python-shell-extra-pythonpaths "~/repos/madame_web"))

(use-package lsp-pyright
  :after lsp
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

;; Handle different python versions
(use-package pyenv-mode
  :ensure t
  :defer t
  :config
  (pyenv-mode))

;; Remove unused imports on save
(use-package pyimport
  :ensure t
  :config
  (add-hook 'before-save-hook 'pyimport-remove-unused))

;; Sort imports on save
(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

;; Handle venvs
(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; Format buffer on save
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(null-keybinds-major-key-def
  :states '(normal visual)
  :keymaps 'python-mode-map
  "r" 'run-python
  "c" 'python-shell-send-buffer
  "f" 'python-shell-send-defun
  "v" 'python-shell-send-region)

(provide 'null-python)

;;; null-python.el ends here
