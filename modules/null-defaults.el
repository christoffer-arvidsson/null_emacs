;;; null-defaults.el -*- lexical-binding: t; -*-

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
(setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save history
(use-package savehist
  :init
  (savehist-mode))

(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil)
  :config
  (super-save-mode +1))

(setq split-width-threshold 0)
(setq split-height-threshold  nil)

(provide 'null-defaults)
