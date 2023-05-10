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
  (lsp-enable-semantic-hightlighting nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.500)
  (lsp-file-watch-threshold 50000)
  (lsp-lens-enable nil)

  (lsp-completion-provider :capf)

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
  :keymaps 'lsp-mode-map
  "l"  '(:ignore t :wk "lsp")
  "l d" 'xref-find-definitions
  "l h" 'lsp-describe-thing-at-point
  "l r" 'lsp-rename
  "l R" 'xref-find-references
  "l n" 'lsp-ui-find-next-reference
  "l p" 'lsp-ui-find-prev-reference
  "l e" 'lsp-ui-flycheck-list
  "l S" 'lsp-ui-sideline-mode)

(provide 'null-lsp)
;;; null-lsp.el ends here
