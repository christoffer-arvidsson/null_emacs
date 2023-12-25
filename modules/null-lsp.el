;;; null-lsp.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'null-keybinds)

(use-package eglot
  :ensure t
  :after cape
  :straight (:type built-in)
  :hook
  (c-mode . eglot-ensure)
  (c-ts-base-mode . eglot-ensure)
  (c++-ts-base-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (python-base-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :config
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       (cape-company-to-capf #'company-yasnippet)))))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'eglot-server-programs '((cuda-mode c++-mode c-mode c++-ts-mode c++-ts-base-mode) "clangd")))

(use-package flymake
  :after eglot
  :straight (:type built-in)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  :ensure t)

(null-keybinds-leader-key-def
  :keymaps 'eglot-mode-map
  "l a" '(eglot-code-actions :wk "code action")
  "l R" '(eglot-rename :wk "rename")
  "l d" '(xref-find-definitions :wk "find definitions")
  "l r" '(xref-find-references :wk "find references")
  "l h" '(eldoc :wk "help")
  "l r" '(eglot-find-declaration :wk "find declaration")
  "l i" '(eglot-find-implementation :wk "find implementation")
  "l e" '(consult-flymake :wk "consult errors")
  "l b" '(:ignore t :wk "buffer")
  "l b f" '(eglot-format-buffer :wk "format buffer")
  "l t" '(eglot-find-typeDefinition :wk "find typeDefinition")
  "l H" '(eglot-inlay-hints-mode :wk "toggle inlay hints"))

(provide 'null-lsp)
;;; null-lsp.el ends here
