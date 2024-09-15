;;; null-startup.el -*- lexical-binding: t; -*-

;;; Code:

(defun null/dashboard-banner ()
  "Set a dashboard banner including information on package initialization
       time and garbage collections."
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

  (defun dashboard-get-icon-str (heading &optional icon)
    (let ((args `( :height   ,dashboard-heading-icon-height
                   :v-adjust ,dashboard-heading-icon-v-adjust
                   :face     dashboard-heading)))
      (pcase heading
        ("Recent Files:"
         (apply #'dashboard-octicon (cdr (assoc 'recents dashboard-heading-icons)) args))
        ("Bookmarks:"
         (apply #'dashboard-octicon (cdr (assoc 'bookmarks dashboard-heading-icons)) args))
        ((or "Agenda for today:"
             "Agenda for the coming week:")
         (apply #'dashboard-octicon (cdr (assoc 'agenda dashboard-heading-icons)) args))
        ("Registers:"
         (apply #'dashboard-octicon (cdr (assoc 'registers dashboard-heading-icons)) args))
        ("Projects:"
         (apply #'dashboard-octicon (cdr (assoc 'projects dashboard-heading-icons)) args))
        ("List Directories:"
         (apply #'dashboard-octicon (cdr (assoc 'ls-directories dashboard-heading-icons)) args))
        ("List Files:"
         (apply #'dashboard-octicon (cdr (assoc 'ls-files dashboard-heading-icons)) args))
        (_
         (if (null icon) " " icon)))))

  (defun dashboard-insert-heading (heading &optional shortcut icon)
    "Insert a widget HEADING in dashboard buffer, adding SHORTCUT, ICON if provided."
    (when (and (dashboard-display-icons-p) dashboard-set-heading-icons)
      (let ((icon-str (dashboard-get-icon-str heading icon)))
        (insert icon-str)
        (insert (make-string (+ 1 (string-width icon-str)) ?\s))))

      ;; Insert heading after the icon
      (insert (propertize heading 'face 'dashboard-heading))

      ;; Turn the inserted heading into an overlay
      (let ((ov (make-overlay (- (point) (length heading)) (point) nil t)))
        (overlay-put ov 'display (or (cdr (assoc heading dashboard-item-names)) heading))
        (overlay-put ov 'face 'dashboard-heading))

      ;; Add the shortcut if provided
      (when shortcut
        (insert (format dashboard-heading-shorcut-format shortcut))))

    (dashboard-setup-startup-hook)

    ;; Makes emacsclient default to the dashboard
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

  (provide 'null-startup)
;;; null-startup.el ends here
