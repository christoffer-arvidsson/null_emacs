;;; null-lsp.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'null-keybinds)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (python-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (cc-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point)
              ("M-RET" . lsp-execute-code-action))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.500)
  (lsp-file-watch-threshold 50000)

  (lsp-completion-provider :none)

  ; Rust
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-position 'bottom)
  :config
  (lsp-ui-doc-show))

(null-keybinds-leader-key-def
  "l"  '(:ignore t :wk "lsp")
  "ld" 'xref-find-definitions
  "lh" 'lsp-describe-thing-at-point
  "lr" 'lsp-rename
  "lR" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode)

(provide 'null-lsp)
;;; null-lsp.el ends here
