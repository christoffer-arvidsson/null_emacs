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
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

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

  :init (global-flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 1500))

(use-package deadgrep)

;; Use ripgrep over grep
(grep-apply-setting
 'grep-find-command
 '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

(null-keybinds-leader-key-def
  :states 'normal
  "p" '(:ignore t :wk "project")
  "p c" '(project-compile :wk "Compile project")
  "p f" '(project-find-file :wk "Find file in project")
  "p p" '(project-switch-project :wk "Switch project")
  "p b" '(project-switch-to-buffer :wk "Switch to project buffer")
  "p ." '(project-dired :wk "Open dired in projcet")
  "p s" '(deadgrep :wk "Search project with deadgrep")
  "/" '(consult-ripgrep :wk "Search project")

  "g" '(:ignore t :wk "git")
  "g B" '(magit-blame :wk "Magit blame")
  "g g" '(magit :wk "Magit status")
  "g t" '(git-timemachine :wk "Git timemachine")
  "g m" '(null/smerge-body :wk "Smerge hydra"))

(provide 'null-project)
