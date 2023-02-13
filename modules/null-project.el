;;; Code:

;; Blazingly fast
(use-package rg
    :ensure t
    :config
   (rg-enable-default-bindings))

;; Dumb jump for jumping to symbol
(use-package dumb-jump
    :ensure t
    :custom
    (dumb-jump-prefer-searcher 'rg)
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Use projcet.el
;; Defines a "m" option to open magit
(use-package project
  :config
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;;; Version control

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setenv "SSH_AUTH_SOCK" "/run/user/1400946984/ssh-agent.socket"))

(use-package hydra)
(use-package smerge-mode
  :config
  (defhydra scimax-smerge (:color red :hint nil)
    "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("c" smerge-keep-current)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("b" smerge-keep-base)
    ("a" smerge-keep-all)
    ("e" smerge-ediff)
    ("j" previous-line)
    ("k" forward-line)
    ("r" smerge-refine)
    ("u" undo)
    ("q" nil :exit t))

  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)
          (scimax-smerge/body))))))

(use-package git-gutter-fringe
  :config
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :after magit
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :init (global-flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 1500))

;; Treesitter
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure tree-sitter)



(null-keybinds-leader-key-def
  :keymaps 'normal
  "p" '(:ignore t :wk "project")
  "p c" '(project-compile :wk "Compile project")
  "p f" '(project-find-file :wk "Find file in project")
  "p p" '(project-switch-project :wk "Switch project")
  "p b" '(project-switch-to-buffer :wk "Switch to project buffer")
  "p s" '(consult-ripgrep :wk "Search project")
  "/" '(consult-ripgrep :wk "Search project")

  "g" '(:ignore t :wk "git")
  "g B" '(magit-blame :wk "Magit blame")
  "g g" '(magit :wk "Magit status")
  "g t" '(git-timemachine :wk "Git timemachine")
  "g m" '(null/smerge-body :wk "Smerge hydra"))

(provide 'null-project)
