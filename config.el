;;; Code:

;; core
(require 'null-startup)
(require 'null-ui)
(require 'null-keybinds)
(require 'null-defaults)

;; buffer, files, windows
(require 'null-files)
(require 'null-windows)
(require 'null-project)

;; editor
(require 'null-editor)
(require 'null-completion)
(require 'null-lsp)

;; org
(require 'null-org)
(require 'null-org-agenda)
(require 'null-org-knowledge)

;; languages
(require 'null-elisp)
(require 'null-rust)

;; Personal information
(setq user-full-name "Christoffer Arvidsson"
      user-mail-address "christoffer@arvidson.nu")

(provide 'config)
;;; config.el ends here

