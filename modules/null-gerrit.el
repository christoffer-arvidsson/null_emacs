;;; null-gerrit.el --- summary -*- lexical-binding: t -*-

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

(use-package zuul
  :ensure t)
(use-package egerrit
  :straight (:type git :host nil :repo "https://git.sr.ht/~niklaseklund/egerrit")
  :ensure transient
  :commands egerrit-dashboard
  :custom
  (egerrit-request-url "https://gerrit.cicd.autoheim.net")
  (egerrit-comment-block-list '("^recheck" "^regate"))
  (egerrit-project-configs '((:name "src" :code-repo "~/repos/src" :review-repo "~/repos/src-review")
                            (:name "madame-web" :code-repo "~/repos/madame-web" :review-repo "~/repos/madame-web-review")))
  :config
  (evil-set-initial-state 'egerrit-dashboard-mode 'emacs))

(use-package zenutil
  :straight (:type git :local-repo "~/.config/emacs/zenemacs/packages/zenutil")
  :defer t)

(use-package zengerrit
  :straight (:type git :local-repo "~/.config/emacs/zenemacs/packages/zengerrit")
  :after egerrit

  :config
  ;; The certificate file is important in order for the Zuul integration to work
  (setq zengerrit-zuul-ca-certificate
        (let* ((ca-certificate-directory "~/repos/src/support/ci_helpers")
               (default-directory ca-certificate-directory))
          (thread-last (directory-files ".")
                       (seq-find
                        (lambda (file)
                          (string-match "\.crt" file)))
                       (expand-file-name))))
  (zengerrit-setup))

(null-keybinds-leader-key-def
  :keymaps 'normal
  "g z" '(egerrit-dashboard :wk "Magit status"))

(provide 'null-gerrit)

;;; null-gerrit.el ends here
