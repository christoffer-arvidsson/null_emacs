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

(setq split-height-threshold 120
      split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

(use-package satchel
  :custom
  (satchel-directory (no-littering-expand-var-file-name "satchel/")))

(null-keybinds-leader-key-def
  :states 'normal
  "'" '(:ignore t :wk "satchel")
  "' s" '(satchel-pick :wk "Pick file")
  "' '" '(satchel-feeling-lucky :wk "Lucky swap file")
  "' X" '(satchel-burn :wk "Burn satchel")
  "' a" '(satchel-place :wk "Add file")
  "' d" '(satchel-demote :wk "Demote file")
  "' u" '(satchel-promote :wk "Demote file")
  "' x" '(satchel-drop :wk "Drop file"))


(provide 'config)
;;; config.el ends here

