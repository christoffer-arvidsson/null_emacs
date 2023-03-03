;;; null-defaults.el -*- lexical-binding: t; -*-

(require 'null-keybinds)

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(setq display-line-numbers-type 'relative)

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
(setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Save history
(use-package savehist
  :init
  (savehist-mode))

(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 10)
  (auto-save-default nil)
  :config
  (super-save-mode +1))

(setq split-width-threshold 0)
(setq split-height-threshold  nil)

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Open pdfs with zathura
(use-package openwith
  :custom
  (openwith-associations '((
                            "\\.pdf\\'" "zathura" (file)
                            )))
  :config
  (openwith-mode t))

;; Delete trailing junk
(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hooks 'no-junk-please-were-unixish)

;; Helpful
(use-package helpful
  :ensure t)

;; Scroll compilation buffers to end
(setq compilation-scroll-output t)

(general-define-key
 :states 'normal
 "C-h f" 'helpful-callable
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key
 "K" 'helpful-at-point)

(null-keybinds-leader-key-def
  :states 'normal
  "h" '(:ignore t :wk "help")
  "h f" 'helpful-callable
  "h v" 'helpful-variable
  "h k" 'helpful-key)

(provide 'null-defaults)


