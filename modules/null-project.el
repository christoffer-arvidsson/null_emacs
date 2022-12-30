;;; Code:

(require 'consult)

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

(use-package magit
  :config
  (setenv "SSH_AUTH_SOCK" "/run/user/1400946984/ssh-agent.socket"))

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
  "g g" '(magit :wk "Magit status"))

(provide 'null-project)
