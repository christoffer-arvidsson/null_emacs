;;; null-org-babel.el --- Notebooking module -*- lexical-binding: t -*-

;; Author: Christoffer Arvidsson
;; Maintainer: Christoffer Arvidsson
;; Version: 1.0
;; Package-Requires: (org)
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

;; Module for a nice org babel setup and thus notebooking.

;;; Code:
(require 'null-org)
(require 'null-org-knowledge)

(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")

(defcustom null/org-babel-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (C . t)
    (gnuplot . t))
  "Languages babel should load.")

(use-package jupyter
  :after (:all org python zmq ob-jupyter)
  :config
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))

  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)
  (setq org-babel-python-command "~/.pyenv/shims/python")
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:kernel . "python3")
                                                       (:exports . "both")
                                                       (:pandoc t)
                                                       (:session . "py")
                                                       (:eval . "never-export")))

  (setq ob-async-no-async-languages-alist '("jupyter-python"))
  (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names)
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
  (add-to-list 'org-structure-template-alist '("ju" . "src jupyter-python")))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages null/org-babel-languages)
  (org-babel-lob-ingest (expand-file-name "templates/lob.org" null/orbit-directory)))

;; Had to to this to properly use this function.
;; This is nice to have as it makes github recognize the code blocks as python.
;; Plus, I have no use for normal python blocks anyway

(with-eval-after-load 'ob-jupyter
  (add-to-list null/org-babel-languages (jupyter . t))
  (org-babel-do-load-languages 'org-babel-load-languages null/org-babel-languages)
  (org-babel-jupyter-override-src-block "python"))

(provide 'null-org-babel)

;;; null-org-babel.el ends here
