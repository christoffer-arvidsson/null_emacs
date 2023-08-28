;;; null-ui.el -*- lexical-binding: t; -*-

(require 'null-keybinds)

(defvar null-theme 'doom-horizon
  "The default theme.")

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
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons-completion
  :ensure all-the-icons
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-horizon-brighter-comments t)
  (doom-horizon-comment-bg nil)
  (doom-themes-padded-modeline t)
  :config
  (doom-themes-org-config))

;; brighter line numbers
(custom-set-faces
 `(line-number ((t (:foreground ,(doom-color 'magenta)))))
 `(org-special-keyword ((t (:foreground ,(doom-color 'magenta)))))
 `(org-property-value ((t (:foreground ,(doom-color 'magenta))))))


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
              (lambda (frame)
                (with-selected-frame frame
                  (fontaine-set-preset null-font-preset))))
  (fontaine-set-preset null-font-preset))

(null-keybinds-leader-key-def
  :states 'normal
  "q t" '(consult-theme :wk "Switch theme")
  "t b" '(lambda () (interactive) (null-global-big-text-mode 'toggle)))

(provide 'null-ui)
