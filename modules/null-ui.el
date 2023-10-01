;;; null-ui.el -*- lexical-binding: t; -*-

(require 'null-keybinds)

(defvar null-theme 'ef-winter

(defvar null-font-preset 'default
  "Fontaine preset to use.")

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
  (setq visible-bell nil)
  (setq left-fringe-width 16)
  (menu-bar-mode -1))

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; initialize ui
(null-ui-init)

(use-package ef-themes
  :ensure t)

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

(use-package fontaine
  :ensure t
  :custom
  (fontaine-presets
   '(
     (default
      :default-family "Monospace")
     (desktop
      :default-family "Iosevka"
      :default-weight normal
      :default-height 110
      :variable-pitch-family "Vollkorn"
      :variable-pitch-weight normal
      :variable-pitch-height 1.05
      :line-spacing nil)
     (big
      :inherit desktop
      :default-height 150)
     (laptop
      :inherit desktop
      :default-height 95))))

(add-hook 'after-init-hook (lambda () (load-theme null-theme t)))

(define-minor-mode null-global-big-text-mode
  "Toggle big text mode."
  :init-value nil
  :global t
  :group 'null
  :lighter " big-text"
  (if null-global-big-text-mode
      (progn
        (message "big-text-mode activated!")
        (fontaine-set-preset 'big))
    (progn (message "big-text-mode deactivated!")
           (fontaine-set-preset null-font-preset))))

;; Required so that emacs client changes font
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (defun null/font-init-daemon (frame)
                (with-selected-frame frame
                  (fontaine-set-preset null-font-preset))
                (remove-hook 'after-make-frame-functions
                             #'null/font-init-daemon)
                (fmakeunbound 'null/font-init-daemon)))
  (fontaine-set-preset null-font-preset))

(null-keybinds-leader-key-def
  :states 'normal
  "q t" '(consult-theme :wk "Switch theme")
  "t b" '(lambda () (interactive) (null-global-big-text-mode 'toggle)))

(provide 'null-ui)
