;;; Code:

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
(require 'null-org)
(require 'null-org-agenda)
(require 'null-org-knowledge)

;; languages
(require 'null-cpp)
(require 'null-elisp)
(require 'null-python)
(require 'null-rust)

(defun eethern/init_work_packages ()
  "Do work specific initialization."
  (message "using work configuration."))

;; Work
(if (string-equal (system-name) "ucnd1387l61") (eethern/init_work_packages))

;; Personal information
(setq user-full-name "Christoffer Arvidsson"
      user-mail-address "christoffer@arvidson.nu")

(provide 'config)
;;; config.el ends here

