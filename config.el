;;; Code:

;; core
(require 'null-defaults)
(require 'null-keybinds)
(require 'null-font)
(require 'null-ui)
(require 'null-startup)

;; buffer, files, windows
(require 'null-files)
(require 'null-project)
(require 'null-windows)
(require 'null-pike)

;; editor
(require 'null-completion)
(require 'null-editor)
(require 'null-treesit)
(require 'null-snippets-tempel)
(require 'null-lsp)

;; org
(require 'null-latex)
(require 'null-org)
(require 'null-org-agenda)
(require 'null-org-knowledge)
(require 'null-org-latex)
(require 'null-org-html)
(require 'null-denote)

;; languages
(require 'null-cpp)
(require 'null-elisp)
(require 'null-python)
(require 'null-rust)
(require 'null-sql)

(require 'null-org-babel)
;; (require 'null-org-publish)

;; other
(require 'null-docker)
(require 'null-apps)
(require 'null-rss)

(elpaca-wait)

;; Personal information
(setq user-full-name "Christoffer Arvidsson"
      user-mail-address "christoffer@arvidson.nu")

;; Theme
(setq null-theme 'ef-winter)

(defun null/init-work-config ()
  "Do work specific initialization."
  (message "using work configuration.")
  (require 'secrets)

  (use-package ox-confluence-modern
    :ensure (:host github :repo "nan0scho1ar/ox-confluence-modern" :files ("*.el")))

  (setq null-font-preset 'laptop
        null-font-big-preset 'laptop-big))

(defun null/init-shuttle-config ()
  "Do work specific initialization."
  (message "using shuttle configuration.")

  (setq null-font-preset 'laptop
        null-font-big-preset 'laptop-big))

(defun null/init-home-config ()
  "Do home specific initialization."
  (message "using home configuration.")
  (setq null-font-preset 'desktop
        null-font-big-preset 'big))

(pcase (system-name)
  ("ucnd1387l61" (null/init-work-config))
  ("station" (null/init-home-config))
  ("shuttle" (null/init-shuttle-config))
  (_ (message "No matching system configuration")))

;; Reload the font
(fontaine-set-preset null-font-preset)

(elpaca-wait)
(load-theme null-theme t)

(provide 'config)
;;; config.el ends here
