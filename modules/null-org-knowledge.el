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

(defconst eethern/citar-path (concat eethern/org-directory "bibliography")
  "Path to citar base directory")

(defconst eethern/citar-bibliography-path (expand-file-name "references.bib" eethern/citar-path)
  "Path to file bibliography file.")

(use-package citar
  :no-require
  :bind (:map minibuffer-local-map
              ("M-b" . citar-insert-preset))
  :custom
  (citar-notes-paths '(eethern/citar-path))
  (citar-bibliography '(eethern/citar-bibliography-path))
  (org-cite-global-bibliography '(eethern/citar-bibliography-path))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  "))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config
  (setq citar-org-roam-note-title-template "${author editor} :: ${title}")
  (citar-org-roam-mode))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/orbit/articles"))
  (+org-roam-open-buffer-on-find-file nil)
  (org-roam-auto-replace-fuzzy-links nil)
  (org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode t)

  (org-roam-node-display-template (format "${title:*} %s %s"
                                          (propertize "${tags:10}" 'face 'font-lock-keyword-face)
                                          (propertize "${file:48}" 'face 'org-tag)))
  :config
  (setq org-roam-capture-templates
        '(("d" "temporary note" plain
           (file "~/Dropbox/org/orbit/templates/draft.org")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("l" "lecture note" plain
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
           :unnarrowed t))))

(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(null-keybinds-leader-key-def
  :keymaps 'normal
  "n r" '(:ignore t :wk "Org roam")
  "n r f" '(org-roam-node-find :wk "Find node")
  "n r r" '(org-roam-node-random :wk "Random node") 
  "n r c" '(org-roam-capture :wk "Capture note")
  "n r t" '(org-roam-tag-add :wk "Add tag")
  "n r a" '(org-roam-alias-add :wk "Add alias")
  "n r C" '(citar-open :wk "Find cite note")
  "n r s" '(org-roam-buffer-toggle :wk "Toggle org roam status buffer")
  "n r S" '(org-roam-db-sync :wk "Sync roam database")
  "n r u" '(org-roam-ui-open :wk "Open org roam ui")
  "n r i" '(org-roam-node-insert :wk "Insert node"))


(provide 'null-org-knowledge)

;;; null-org-knowledge.el ends here
