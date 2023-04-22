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
  (add-to-list 'company-backends 'company-capf)

  ;; Company yas
  (with-eval-after-load 'yasnippet
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (with-eval-after-load 'company
      (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))))


;; Required for proportional font
(use-package company-posframe
  :after company
  :config
  (company-posframe-mode 1))

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


;; Fix company with yasnippet

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(with-eval-after-load 'company
  (defun company-mode/backend-with-yas (backends)
    (if (or (not company-mode/enable-yas) (and (listp backends) (member 'company-yasnippet backends)))
        backends
      (append (if (consp backends) backends (list backends))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; From here. Dated 2015, tested 2023. API use confirmed by author of yasnippet
  ;; https://stackoverflow.com/a/28510968
  ;; Try yas-expand and on failure to company-completion
  (defun company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-fallback-behavior nil))
      (unless (yas-expand)
        (call-interactively #'company-complete-common))))

  (add-hook 'company-mode-hook (lambda ()
                                 (substitute-key-definition 'company-complete-common
                                                            'company-yasnippet-or-completion
                                                            company-active-map))))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode +1))

(use-package vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode +1))

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
