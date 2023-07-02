;;; null-elisp.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'null-keybinds)

(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t))

(null-keybinds-major-key-def
  :states '(normal visual)
  :keymaps 'emacs-lisp-mode-map
  "e" '(:ignore t :wk "eval")
  "e e" 'eval-last-sexp
  "e l" 'load-library
  "e d" 'eval-defun
  "e b" 'eval-buffer
  "e r" 'eval-region)

(provide 'null-elisp)
;;; null-elisp.el ends here
