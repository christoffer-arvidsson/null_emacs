(defvar native-comp-deferred-compilation-deny-list nil)
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package '(org :local-repo nil))

;;; bootstrap package system
(defvar bootstrap-directory (expand-file-name "bootstrap/" user-emacs-directory)
  "Package system bootstrap configuration.")

(load (expand-file-name "null-package-bootstrap.el" bootstrap-directory))

(provide 'null-package/straight)
