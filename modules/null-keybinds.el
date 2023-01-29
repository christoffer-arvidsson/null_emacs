;;; null-keybinds.el -*- lexical-binding: t; -*-

;;; Code:

;; General leader

(defgroup null-keybinds '()
  "Keybind related configuration."
  :tag "Null keybinds")

(defcustom null-keybinds-leader-key "SPC"
  "The leader key used by general."
  :group 'null-keybinds
  :type 'string)

(defcustom null-keybinds-major-key "SPC m"
  "The major mode key used by general."
  :group 'null-keybinds
  :type 'string)

(defcustom null-keybinds-ctrl-c-key "C-c"
  "The ctrl-c key used by general."
  :group 'null-keybinds
  :type 'string)

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-kill-on-visual-paste nil
        evil-undo-system 'undo-tree
        evil-respect-visual-line-mode t)
  :config
  (let ((after-fn (lambda (&rest _) (recenter nil))))
    (advice-add 'evil-goto-line :after after-fn)
    (advice-add 'evil-goto-mark :after after-fn)
    (advice-add 'evil-goto-mark-line :after after-fn)
    (advice-add 'evil-scroll-down :after after-fn)
    (advice-add 'evil-scroll-up :after after-fn))

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'lispy evil-collection-mode-list)
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))

;; General
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer null-keybinds-leader-key-def
    :keymaps '(normal visual emacs)
    :prefix null-keybinds-leader-key)

  (general-create-definer null-keybinds-major-key-def
    :keymaps '(normal visual emacs)
    :prefix null-keybinds-major-key)

  (general-create-definer null-keybinds-ctrl-c-key-def
    :prefix null-keybinds-ctrl-c-key)
  
  (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
    (let (orig-one-window-p)
      (fset 'orig-one-window-p (symbol-function 'one-window-p))
      (fset 'one-window-p (lambda (&optional nomini all-frames) t))
      (unwind-protect
          ad-do-it
        (fset 'one-window-p (symbol-function 'orig-one-window-p))))))

;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Non-package keys
(null-keybinds-leader-key-def
  :keymaps 'normal
  "" '(nil :wk "Leader")
  "t" '(:ignore t :wk "toggle")
  "u" 'universal-argument
  "RET" '(consult-bookmark :wk "Jump to bookmark")
  "q" '(:ignore t :wk "quit")
  "q r" '(restart-emacs :wk "Restart emacs")
  "q q" '(quit-window :wk "Quit emacs"))

(provide 'null-keybinds)

