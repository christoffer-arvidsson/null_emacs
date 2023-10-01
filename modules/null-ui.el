;;; null-ui.el -*- lexical-binding: t; -*-

(require 'null-keybinds)
(require 'null-font)

(defvar null-theme 'ef-winter

  "The default theme.")

(defun null-ui-disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'server-after-make-frame-functions 'my/disable-scroll-bars)

(defun null-ui-init ()
  (scroll-bar-mode -1)
  (global-hl-line-mode +1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (column-number-mode +1)
  ;; (setq-default display-line-numbers-width 3)
  (setq visible-bell nil
        left-fringe-width 3
        right-fringe-width 4)
  (menu-bar-mode -1))

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; initialize ui
(null-ui-init)

(use-package ef-themes
  :ensure t
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui nil))


;; icons
(use-package nerd-icons
  :after kind-icon
  :ensure t
  :config
  (setq nerd-icons-font-family "Iosevka Nerd Font"))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :straight (:type git :host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after (nerd-icons corfu)
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode)
  :ensure t)

(use-package mood-line
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode)
  :config
  (mood-line-mode))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(add-hook 'after-init-hook (lambda () (load-theme null-theme t)))

(null-keybinds-leader-key-def
  :states 'normal
  "q t" '(consult-theme :wk "Switch theme")
  "t b" '(lambda () (interactive) (null-global-big-text-mode 'toggle)))

(provide 'null-ui)
