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
(require 'null-treesit)
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
(require 'null-sql)

(require 'null-org-babel)

;; other
(require 'null-apps)
(require 'null-rss)

;; Personal information
(setq user-full-name "Christoffer Arvidsson"
      user-mail-address "christoffer@arvidson.nu")

;; Theme
(setq null-theme 'ef-winter)

(defun null/init-work-config ()
  "Do work specific initialization."
  (message "using work configuration.")
  (setq null-font-preset 'laptop))

(defun null/init-home-config ()
  "Do home specific initialization."
  (message "using home configuration.")
  (setq null-font-preset 'desktop))

(let ((name (system-name)))
  (pcase name
    ("ucnd1387l61" (null/init-work-config))
    ("station" (null/init-home-config))
    (_ (message "No matching system configuration"))))

;; Reload the font
(fontaine-set-preset null-font-preset)

(provide 'config)
;;; config.el ends here
