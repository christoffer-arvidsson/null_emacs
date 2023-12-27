;;; null-completion.el -*- lexical-binding: t; -*-

;;; Code:
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :after orderless
  :general (:states 'normal :keymaps 'corfu-mode-map
                    "M-m" #'corfu-move-to-minibuffer)
  :custom
  (corfu-on-exacs-match nil)
  (corfu-cycle t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 3)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  ;; popupinfo
  (corfu-popupinfo-delay 0.5)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  (corfu-echo-mode +1)
  (corfu-popupinfo-mode +1)
  :init
  (global-corfu-mode)

  ;; https://github.com/emacs-evil/evil-collection/issues/766
  (advice-remove 'corfu--setup 'evil-normalize-keymaps)
  (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
  (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; delete me once yas has a good cape adapter
(use-package company
  :after cape corfu
  :ensure t)

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3))

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles basic partial-completion))))
  (orderless-component-separator "[ &]"))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  :bind (:map minibuffer-local-map
              ("M-s". consult-history)
              ("M-r". consult-history))
  :config
  ; fd support
  (defun consult--fd-builder (input)
    (let ((fd-command
           (if (eq 0 (process-file-shell-command "fdfind"))
               "fdfind"
             "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
        (when re
          (cons (append
                 (list fd-command
                       "--color=never" "--full-path"
                       (consult--join-regexps re 'extended))
                 opts)
                hl)))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))

  ; Add selected text to initial query on search
  (consult-customize consult-line
                     consult-ripgrep
                     :preview-key '(:debounce 0.0 any)
                     :add-history (seq-some #'thing-at-point '(region symbol))
                     :initial (if (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                                (thing-at-point 'symbol))))

(use-package embark
  :after vertico
  :general (:keymaps 'vertico-map
            "C-." 'embark-act
            "C-," 'embark-export
            "M-." 'embark-dwim
            "C-h B" 'embark-bindings)
  :ensure t)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :ensure t
  :config
  (corfu-prescient-mode +1))

(use-package vertico-prescient
  :after (vertico prescient)
  :config
  (vertico-prescient-mode +1))

(use-package emacs
  :hook (minibuffer-setup-hook #'cursor-intangible-mode)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(provide 'null-completion)
