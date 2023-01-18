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
(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it. 
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :after consult
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  
  :config
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(use-package winner
  :config
  ;; Allow restoring window changes
  (winner-mode +1))

;;; Bindings

(general-define-key
 :states 'normal
 "<prior>" 'evil-scroll-up
 "<next>" 'evil-scroll-down)

(null-keybinds-leader-key-def
  :keymaps 'normal
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

  ;; For standard vi bindings (incase of non-colemak kb)
  "w h" '(evil-window-left :wk "Select window left")
  "w j" '(evil-window-down :wk "Select window down")
  "w k" '(evil-window-up :wk "Select window up")
  "w l" '(evil-window-right :wk "Select window right")
  "w H" '(+evil/window-move-left :wk "Move window left")
  "w J" '(+evil/window-move-down :wk "Move window down")
  "w K" '(+evil/window-move-up :wk "Move window up")
  "w L" '(+evil/window-move-right :wk "Move window right")

  ;; Workspace
  "W" '(nil :wk "workspace")
  "W TAB" '(tab-previous :wk "previous workspaces")
  "W C" '(tabspaces-clear-buffers :wk "clear workspace buffers")
  "W b" '(tabspaces-switch-to-buffer :wk "switch workspace buffer")
  "W x" '(tabspaces-close-workspace :wk "close workspace")
  "W X" '(tabspaces-kill-buffers-close-workspace :wk "kill buffers and close workspace")
  "W o" '(tabspaces-open-or-create-project-and-workspace :wk "open or create project workspace")
  "W d" '(tabspaces-remove-current-buffer :wk "remove current buffer")
  "W D" '(tabspaces-remove-current-buffer :wk "remove selected buffer")
  "W s" '(tabspaces-switch-or-create-workspace :wk "switch or create workspace")
  "W t" '(tabspaces-switch-buffer-and-tab :wk "switch buffer and tab"))

(provide 'null-windows)
