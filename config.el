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
(setq null-theme 'ef-dream)

(defun null/init-work-config ()
  "Do work specific initialization."
  (message "using work configuration.")
  (require 'secrets)

  (use-package copilot
    :ensure (:host github :repo "copilot-emacs/copilot.el")
    :custom
    (copilot-idle-delay 0.5)
    :general (:keymaps 'copilot-completion-map
                       "M-]" 'copilot-accept-completion))

  (use-package ox-slack
    :ensure (:host github :repo "titaniumbones/ox-slack"))

  (setq null-font-preset 'laptop
        null-font-big-preset 'laptop-big))

(defun null/init-shuttle-config ()
  "Do work specific initialization."
  (message "using shuttle configuration.")

  (setq null-font-preset 'laptop
        null-font-big-preset 'laptop-big))

  (use-package gptel
    :ensure t
    :config
    (setq-default
     gptel-model "llama3.2"
     gptel-backend (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :stream t
                     :models '("llama3.2")))
    (setq gptel-log-level 'debug))


(defun null/init-home-config ()
  "Do home specific initialization."
  (message "using home configuration.")
  (setq null-font-preset 'desktop
        null-font-big-preset 'big))

(pcase (system-name)
  ("u5cg4373yhk" (null/init-work-config))
  ("station" (null/init-home-config))
  ("shuttle" (null/init-shuttle-config))
  (_ (message "No matching system configuration")))

;; Reload the font
(fontaine-set-preset null-font-preset)

(elpaca-wait)
(load-theme null-theme t)

(provide 'config)
;;; config.el ends here
