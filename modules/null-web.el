(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package web-mode
  
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))))

(use-package typescript-mode
  
  :hook (typescript-mode . subword-mode))
  :config
  (setq typescript-indent-level 2)

(use-package tide
  :init
  
  :after (typescript-mode)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (before-save . tide-format-before-save))

(provide 'null-web)
