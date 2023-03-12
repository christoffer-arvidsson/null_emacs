;;; null-ui.el -*- lexical-binding: t; -*-

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
  (menu-bar-mode -1))

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; initialize ui
(null-ui-init)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-horizon-brighter-comments t)
  (doom-horizon-comment-bg nil)
  (doom-themes-padded-modeline t)
  :config

  (load-theme 'doom-horizon t)
  (doom-themes-org-config))

;; brighter line numbers
(custom-set-faces
 `(line-number ((t (:foreground ,(doom-color 'magenta)))))
 `(org-special-keyword ((t (:foreground ,(doom-color 'magenta)))))
 `(org-property-value ((t (:foreground ,(doom-color 'magenta))))))

;; icons
(use-package all-the-icons
  :ensure t
  :custom
  (doom-modeline-icon t))

(use-package all-the-icons-completion
  :ensure all-the-icons
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name t)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-buffer-file-name-style 'truncate-except-project))

(use-package solaire-mode
  :defer t
  :ensure t
  :config
  (solaire-global-mode +1))

(defun null-ui-set-font-faces ()
  (set-face-attribute 'default nil
                      :font "Iosevka"
                      :weight 'normal
                      :height 100)

  (set-face-attribute 'fixed-pitch nil
                      :font "Vollkorn"
                      :weight 'normal
                      :height 150)

  (set-face-attribute 'variable-pitch nil
                      :font "Vollkorn"
                      :weight 'normal
                      :height 150))

;; Required so that emacs client changes font
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (null-ui-set-font-faces))))
  (null-ui-set-font-faces))

(provide 'null-ui)

