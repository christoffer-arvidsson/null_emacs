;;; null-windows.el -*- lexical-binding: t; -*-

(require 'null-keybinds)

;; Code

;;; Functions

(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'evil-window-move-far-left)
                   ('right #'evil-window-move-far-right)
                   ('up    #'evil-window-move-very-top)
                   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (doom-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

(defun null-windows-scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (null-windows-scroll-half-page nil))

(defun null-windows-scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (null-windows-scroll-half-page t))

;; Packages
(use-package winner
  :config
  ;; Allow restoring window changes
  (winner-mode +1))

(use-package harpoon
  :custom
  (harpoon-separate-by-branch nil)
  (harpoon-project-package 'project)
  :bind
  (("M-n" . harpoon-go-to-1)
   ("M-e" . harpoon-go-to-2)
   ("M-i" . harpoon-go-to-3)
   ("M-o" . harpoon-go-to-4)))

(use-package shackle
  :commands shackle-mode
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules
        '(("*org-roam*"                   :align right  :size 0.3    :select t)
          ("*org-roam-review*"            :align right  :size 0.3    :select t)
          ("\\`\\*Embark Export. *\\*\\'" :regexp t     :align right :size 0.3 :select t)
          ("\\`\\*Embark Collect.*\\*\\'" :regexp t     :align right :size 0.3 :select t)
          ("*Org Links*"                  :noselect nil :size 0.1)
          ("*Backtrace*"                  :noselect t   :align below :size 0.25)
          ("*Warnings*"                   :noselect t   :align below :size 0.25)
          ("*Error*"                      :noselect t   :size 0.25)
          ("*Flycheck errors*"            :noselect t   :size 0.25)
          ("*compilation*"                :noselect t   :size 0.25)
          (compilation-mode               :noselect t   :size 0.25)
          (messages-buffer-mode           :noselect t   :align below :size 0.25)
          (help-mode                      :align below  :select t)
          (helpful-mode                   :align below)
          (magit-status-mode              :align right  :inhibit-window-quit t)
          (magit-log-mode                 :same t       :inhibit-window-quit t)
          (magit-commit-mode              :align below)
          (magit-diff-mode                :select nil   :align left  :size 0.5)
          (git-commit-mode                :align below  :same t)
          (vc-annotate-mode               :same t))))

;;; Bindings

(general-define-key
 :states 'normal
 "<prior>" 'evil-scroll-up
 "<next>" 'evil-scroll-down)

(null-keybinds-leader-key-def
  :states 'normal
  "TAB" '(evil-switch-to-windows-last-buffer :wk "Previous buffer")
  "w" '(nil :wk "window")
  "w m" '(evil-window-left :wk "Select window left")
  "w n" '(evil-window-down :wk "Select window down")
  "w e" '(evil-window-up :wk "Select window up")
  "w i" '(evil-window-right :wk "Select window right")
  "w M" '(+evil/window-move-left :wk "Move window left")
  "w N" '(+evil/window-move-down :wk "Move window down")
  "w E" '(+evil/window-move-up :wk "Move window up")
  "w I" '(+evil/window-move-right :wk "Move window right")
  "w v" '(evil-window-vsplit :wk "Vertical window split")
  "w s" '(evil-window-split :wk "Horizontal window split")
  "w u" '(winner-undo :wk "Undo layout change")
  "w r" '(winner-redo :wk "Redo layout change")
  "w q" '(evil-quit :wk "Evil quit")
  "w =" '(balance-windows :wk "Balance windows")

  "'" '(:ignore t :wk "harpoon")
  "' a" '(harpoon-add-file :wk "Add file"))





(provide 'null-windows)
