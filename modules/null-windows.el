;;; null-windows.el -*- lexical-binding: t; -*-

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


(defun null-windows-scroll-half-page (direction)
  "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)  ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))  ;; Current line becomes first
    (move-to-window-line opos)))  ;; Restore cursor/point position

(defun null-windows-scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (eethern/scroll-half-page nil))

(defun null-windows-scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (eethern/scroll-half-page t))

(general-define-key
 :states 'normal
 "<prior>" 'null-windows-scroll-half-page-up
 "<next>" 'null-windows-scroll-half-page-down
 )

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
  )

(provide 'null-windows)
