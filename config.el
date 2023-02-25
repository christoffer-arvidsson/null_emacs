;;; Code:

;; Personal information
(setq user-full-name "Christoffer Arvidsson"
      user-mail-address "christoffer@arvidson.nu")

;; core
(require 'null-defaults)
(require 'null-keybinds)
(require 'null-startup)
(require 'null-ui)

;; buffer, files, windows
(require 'null-files)
(require 'null-project)
(require 'null-windows)

;; editor
(require 'null-completion)
(require 'null-editor)
(require 'null-lsp)

;; org
(require 'null-latex)
(require 'null-org)
(require 'null-org-agenda)
(require 'null-org-knowledge)
(require 'null-org-latex)

;; languages
(require 'null-cpp)
(require 'null-elisp)
(require 'null-python)
(require 'null-rust)

;; other
(require 'null-apps)

(defun null/init-work-packages ()
  "Do work specific initialization."
  (require 'null-gerrit)

  ;; madame web
  (setq python-shell-interpreter "~/scripts/docker-python-shell.sh")
  (setq python-shell-interpreter-args "")
  (setq python-shell-interpreter-interactive-args "")
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-prompt-detect-failure-warning nil)

  (message "using work configuration."))

;; Work
(if (string-equal (system-name) "ucnd1387l61") (null/init-work-packages))

(provide 'config)
;;; config.el ends here

