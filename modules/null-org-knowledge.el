;;; null-org-knowledge.el --- summary -*- lexical-binding: t -*-

;; Author: Christoffer Arvidsson
;; Maintainer: Christoffer Arvidsson
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'null-org)

(defconst null/orbit-directory "~/Dropbox/org/orbit"
  "Path to orbit directory.")

(defconst null/citar-path (concat null/org-directory "bibliography")
  "Path to citar base directory")

(defconst null/citar-bibliography-path (expand-file-name "references.bib" null/citar-path)
  "Path to file bibliography file.")

(defun convert-file-path-to-relative (file-path fixed-directory)
  "Convert FILE-PATH to a relative path based on FIXED-DIRECTORY."
  (let* ((file-directory (file-name-directory file-path))
         (relative-path (file-relative-name fixed-directory file-directory)))
    (concat relative-path "/")))

(defun null/orbit-org-download-screenshot ()
  "Set `org-download-image-dir' to a relative path based on the current buffer's file path."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (relative-path (convert-file-path-to-relative file-path (expand-file-name "assets/images" null/orbit-directory))))
    (setq-local org-download-image-dir relative-path)
    (org-download-screenshot)
    (message "org-download-image-dir set to: %s" relative-path)))

(defun null/org-download-named-screenshot (fname)
  "Download and save a region directly to FNAME."
  (interactive "FEnter Filename:")
  (make-directory (file-name-directory fname) t)
  (if (functionp org-download-screenshot-method)
      (funcall org-download-screenshot-method fname)
    (shell-command-to-string
     (format org-download-screenshot-method fname)))
  (org-download-image fname))

;; I only use this for saving images to my org-roam buffers. All their images will be stored in
;; ../assets/imagse so that the links to those images will also be relative. This is important
;; for export purposes later, since I don't want any absolute paths to images.
(use-package org-download
  :after org
  :config
  (setq org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s"
        org-download-method 'directory
        org-download-timestamp "%Y-%m-%d_%H-%M-%S_")
  (setq-default org-download-heading-lvl nil))


(use-package citar
  :after org
  :bind (:map minibuffer-local-map
              ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography (list null/citar-bibliography-path))
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths (list null/citar-path))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-roam
  :ensure sqlite
  :after org
  :init
  (setq org-roam-v2-ack t)
  :hook (org-roam-find-file . balance-windows)
  :hook (org-follow-link . balance-windows)
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/orbit/articles"))
  (+org-roam-open-buffer-on-find-file nil)
  (org-roam-auto-replace-fuzzy-links nil)
  (org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode t)

  (org-roam-node-display-template (format "${directories:12} ${title:120} %s %s ${backlinkscount:6}"
                                          (propertize "${tags:30}" 'face 'font-lock-keyword-face)
                                          (propertize "${file:48}" 'face 'org-tag)))
  :config

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-capture-templates
        '(("l" "lecture note" plain
           (file "~/Dropbox/org/orbit/templates/lecture_note.org")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "permanent note" plain
           (file "~/Dropbox/org/orbit/templates/latex.org")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("n" "notebook" plain
           (file "~/Dropbox/org/orbit/templates/notebook.org")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?" :unnarrowed t :target
           (file+head "%<%Y_%m_%d>.org" "#+filetags: :daily:\n#+title: %<%Y-%m-%d>\n\n")))))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} :: ${title}"
        citar-org-roam-capture-template-key "r"))

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   ;; (consult-customize
   ;;  consult-org-roam-forward-links
   ;;  :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package org-drill
  :ensure t
  :after org)

(use-package ht)

(use-package org-format
  :hook (org-mode . org-format-on-save-mode)
  :straight (:type git :host github :repo "chrisbarrett/nursery" :files (:defaults "list/*.el")))

(use-package org-roam-review
  :straight (:type git :host github :repo "chrisbarrett/nursery" :files (:defaults "list/*.el"))
  :after (org-roam org-drill)
  :commands (org-roam-review
             org-roam-review-list-by-maturity
             org-roam-review-list-recently-added)
  :general
  (:states '(normal) :keymaps 'org-roam-review-mode-map
           "TAB" 'magit-section-cycle
           "g r" 'org-roam-review-refresh))

(add-hook 'org-roam-capture-new-node-hook
        (lambda () (org-roam-review-set-seedling)))

(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(defcustom writeroom-text-scale 2.0 "Scale increase for text in writeroom." :type 'integer)

(use-package writeroom-mode
  :after visual-fill-column
  :custom
  (writeroom-width 60)
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-restore-window-config t)
  :config
  ;; adjust writeroom one font resize
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)
  (advice-add 'mouse-wheel-text-scale :after
              #'visual-fill-column-adjust)
  (advice-add 'mouse-wheel-global-text-scale :after
              #'visual-fill-column-adjust)

  ;; big mode
  (add-hook 'writeroom-mode-enable-hook #'(lambda () (text-scale-increase writeroom-text-scale)))
  (add-hook 'writeroom-mode-disable-hook #'(lambda () (text-scale-decrease writeroom-text-scale))))


(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(null-keybinds-leader-key-def
  :states 'normal
  "n r" '(:ignore t :wk "Org roam")
  ;; "n r f" '(org-roam-node-find :wk "Find node")
  "n r /" '(consult-org-roam-search :wk "Search org roam")
  "n r B" '(consult-org-roam-forward-links :wk "List forward links")
  "n r R" '(org-roam-rewrite-rename :wk "Rename node")
  "n r S" '(org-roam-db-sync :wk "Sync roam database")
  "n r a" '(org-roam-alias-add :wk "Add alias")
  "n r b" '(consult-org-roam-backlinks :wk "List backlinks")
  "n r c" '(citar-open :wk "Find cite")
  "n r n" '(citar-open-note :wk "Find cite note")
  "n r p" '(citar-open-files :wk "Find cite pdf")
  "n r f" '(consult-org-roam-file-find :wk "Find node")
  "n r i" '(org-roam-node-insert :wk "Insert node")
  "n r r" '(org-roam-node-random :wk "Random node")
  "n r s" '(org-roam-buffer-toggle :wk "Toggle org roam status buffer")
  "n r t" '(org-roam-tag-add :wk "Add tag")
  "n r u" '(org-roam-ui-open :wk "Open org roam ui")
  "n r T" '(org-transclusion-add :wk "Transclude add")

  "n a" '(:ignore t :wk "Org roam attach")
  "n a c" '(null/orbit-org-download-screenshot :wk "Download screenshot")

  "n d" '(:ignore t :wk "Org roam dailies")
  "n d t" '(org-roam-dailies-capture-today :wk "find today")
  "n d T" '(org-roam-dailies-capture-tomorrow :wk "find tomorrow")
  "n d y" '(org-roam-dailies-capture-yesterday :wk "find yesterday")
  "n d d" '(org-roam-dailies-capture-date :wk "find by date")
  "n d p" '(org-roam-dailies-goto-previous-note :wk "find previous note")
  "n d n" '(org-roam-dailies-goto-next-note :wk "find next note")

  "n e" '(:ignore t :wk "Org roam review")
  "n e a" '(org-roam-review-accept :wk "accept")
  "n e b" '(org-roam-review-set-budding :wk "set budding")
  "n e e" '(org-roam-review-set-evergreen :wk "set evergreen")
  "n e r" '(org-roam-review :wk "review")
  "n e s" '(org-roam-review-set-seedling :wk "set seedling")
  "n e u" '(org-roam-review-bury :wk "bury")
  "n e x" '(org-roam-review-set-excluded :wk "set excluded")

  "t z" '(writeroom-mode :wk "Toggle writeroom mode")
  "t Z" '(global-writeroom-mode :wk "Toggle global writeroom mode")
  "t i" '(org-toggle-inline-images :wk "Toggle inline images"))


(provide 'null-org-knowledge)

;;; null-org-knowledge.el ends here
