;;; null-editor.el -*- lexical-binding: t; -*-

(require 'null-keybinds)
(require 'null-defaults)

(use-package expand-region)

(use-package rainbow-delimiters
  :ghook 'prog-mode-hook)

(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)
         (conf-mode . rainbow-mode)))

(use-package smartparens
  :config
  (smartparens-global-mode))

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode +1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Nerd commenter
(use-package evil-nerd-commenter
  :config
  (general-define-key
   :states 'normal
   "g c" '(evilnc-comment-operator :wk "Comment operator")))

(use-package evil-multiedit
  :ensure t
  :config
  (evil-multiedit-default-keybinds))

(use-package avy
  :custom
  (avy-timeout-seconds 0.3)
  (avy-background t)
  (avy-single-candidate-jump nil)
  :config
  (general-define-key
   :states 'normal
   "g s" 'evil-avy-goto-char-timer)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setq avy-keys '(?q ?e ?w ?u ?o ?a ?s ?f ?g ?h ?j ?k ?l ?' ?c ?v ?b ?n ?, ?/))

  (setf (alist-get ?p avy-dispatch-alist) 'avy-action-yank
        (alist-get ?P avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?d avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?D avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-copy
        (alist-get ?Y avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package iedit
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(provide 'null-editor)
