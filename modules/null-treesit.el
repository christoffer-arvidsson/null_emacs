;;; null-tresit.el --- summary -*- lexical-binding: t -*-

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

(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-extra-load-path (list (concat user-emacs-directory "tree-sitter/")))
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config

  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (r "https://github.com/r-lib/tree-sitter-r")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ; tree-sitter modes
  (setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
                                 (c-mode . c-ts-mode)
                                 (c-or-c++-mode . c-or-c++-ts-mode)
                                 (conf-toml-mode . toml-ts-mode)
                                 (csharp-mode . csharp-ts-mode)
                                 (css-mode . css-ts-mode)
                                 (java-mode . java-ts-mode)
                                 (js-json-mode . json-ts-mode)
                                 (python-mode . python-ts-mode)
                                 (ruby-mode . ruby-ts-mode)
                                 (sh-mode . bash-ts-mode)))

  ; tree-sitter only mode
  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
  
  (global-treesit-auto-mode +1))

(use-package evil-ts
  :straight (:type git :host github :repo "foxfriday/evil-ts"))
;; Treesitter
;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure tree-sitter)

;; (use-package evil-textobj-tree-sitter :ensure t
;;   :after evil
;;   :config
;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;;   ;; You can also bind multiple items and we will match the first one we can find
;;   (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

;;   ;; Goto start of next function
;;   (define-key evil-normal-state-map (kbd "]f") (lambda ()
;;                                                  (interactive)
;;                                                  (evil-textobj-tree-sitter-goto-textobj "function.outer")))
;;   ;; Goto start of previous function
;;   (define-key evil-normal-state-map (kbd "[f") (lambda ()
;;                                                  (interactive)
;;                                                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
;;   ;; Goto end of next function
;;   (define-key evil-normal-state-map (kbd "]F") (lambda ()
;;                                                  (interactive)
;;                                                  (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
;;   ;; Goto end of previous function
;;   (define-key evil-normal-state-map (kbd "[F") (lambda ()
;;                                                  (interactive)
;;                                                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(provide 'null-treesit)

;;; null-tresit.el ends here
