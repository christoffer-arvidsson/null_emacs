;;; null-lsp.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'null-keybinds)

(use-package eldoc
  :ensure t)

(use-package eldoc-box
  :after eldoc
  :ensure t
  :config
  ;; These need to be evil since they have to override evil-collection bindings
  (evil-define-key 'normal 'eglot-mode-map (kbd "K") #'eldoc-box-help-at-point)
  (evil-define-key 'normal 'eglot-mode-map (kbd "K") #'eldoc-box-help-at-point)
  (evil-define-key 'normal 'eglot-mode-map (kbd "C-K") #'eldoc-doc-buffer))

(use-package eglot
  :ensure t
  :after cape
  :hook
  (c-mode . eglot-ensure)
  (c-ts-base-mode . eglot-ensure)
  (c++-ts-base-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (python-base-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :config
  ;; Modes
  (add-to-list 'eglot-server-programs '((c++-ts-mode c++-ts-base-mode cuda-mode c++-mode c-mode) "clangd"))

  ;; Add capfs to eglot
  (defun null/eglot-capf ()
    "Create capf for eglot+cape capfs."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'null/eglot-capf)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Hover documentation
  (add-hook 'eglot-managed-mode-hook
            (lambda () (setq eldoc-documentation-strategy
                             #'eldoc-documentation-compose)))
  (add-hook 'eglot-managed-mode-hook
            (lambda () (setq eldoc-documentation-functions
                             '(flymake-eldoc-function
                               eglot-signature-eldoc-function))))

  ;; Turn off documentation on hover
  (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))))

(use-package flymake
  :after eglot
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
