;;; null-lsp.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'null-keybinds)

(use-package eglot
  :ensure t
  :hook
  (c-mode . eglot-ensure)
  (cpp-ts-base-mode . eglot-ensure)
  (python-base-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (eglot-managed-mode . (lambda ()
                          (add-to-list 'company-backends
                                       '(company-capf :with company-yasnippet))))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode python-base-mode)
                 . ("pyright-langserver" "--stdio"))
               '(cpp-ts-base-mode . "ccls")))

(use-package eldoc-box
  :ensure
  :config
  (eldoc-box-hover-mode +1))

(null-keybinds-leader-key-def
  :keymaps 'eglot-mode-map
  "l"  '(:ignore t :wk "lsp")
  "l a" '(eglot-code-actions :wk "code action")
  "l R" '(eglot-rename :wk "rename")
  "l d" '(xref-find-definitions :wk "find definitions")
  "l r" '(xref-find-references :wk "find references")
  "l h" '(eldoc :wk "help")
  "l r" '(eglot-find-declaration :wk "find declaration")
  "l i" '(eglot-find-implementation :wk "find implementation")
  "l e" '(consult-flymake :wk "consult errors")
  "l f" '(eglot-format-buffer :wk "format buffer")
  "l t" '(eglot-find-typeDefinition :wk "find typeDefinition"))

(provide 'null-lsp)
;;; null-lsp.el ends here
