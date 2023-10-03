;;; null-startup.el -*- lexical-binding: t; -*-

;;; Code:

(defun null/dashboard-banner ()
  """Set a dashboard banner including information on package initialization
       time and garbage collections."""
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

(use-package dashboard
  :after nerd-icons
  :init
  (add-hook 'dashboard-mode-hook 'null/dashboard-banner)
  :custom
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  :config
  (dashboard-setup-startup-hook)

  ;; Hack until `initial-buffer-choice` is not a live buffer
  ;; if fixed upstream
  (get-buffer-create "*dashboard*")
  (dashboard-refresh-buffer)

  ;; Makes emacsclient default to the dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(provide 'null-startup)
;;; null-startup.el ends here
