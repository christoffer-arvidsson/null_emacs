;;; null-completion.el -*- lexical-binding: t; -*-

;;; Code:

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-count 13)
  (vertico-resize nil)
  :config
  (vertico-mode))

(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(use-package orderless
  :after company
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator "[ &]")
  :config
  (advice-add 'company-capf--candidates :around #'just-one-face))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (evil-complete-next-func 'complete-complete-cycle-next)
  (evil-complete-previous-func 'complete-complete-cycle-previous)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)

  :config
  (define-key company-active-map (kbd "<tab>") nil)
  (add-to-list 'company-backends 'company-capf))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-company
  :after (consult company)
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-collect)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(provide 'null-completion)
