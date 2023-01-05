;;; null-org.el --- summary -*- lexical-binding: t -*-

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
(require 'null-keybinds)
(require 'org-tempo)
(require 'org-indent)

(defconst null/org-directory (file-truename "~/Dropbox/org/")
  "Path to org directory.")

(defconst null/org-capture-todo-file (expand-file-name "agenda.org" null/org-directory)
  "Path to file containing personal todos.")

(defconst null/org-capture-work-file (expand-file-name "work.org" null/org-directory)
  "Path to file containing work todos.")

(defconst null/org-capture-work-journal-file (expand-file-name "work_journal.org" null/org-directory)
  "Path to file containing work journal.")

(defconst null/org-capture-journal-file (expand-file-name "journal.org" null/org-directory)
  "Path to file containing personal journal.")

(defconst null/org-drill-file (expand-file-name "drill.org" null/org-directory)
  "Path to file containing drills.")

(defun null/org-toggle-properties ()
  "Toggle visibility of properties in current header if it exists."
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))

(defun null/org--get-foldlevel ()
  (let ((max 1))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (when (outline-invisible-p (line-end-position))
            (let ((level (org-outline-level)))
              (when (> level max)
                (setq max level))))))
      max)))

(defun null/org-show-next-fold-level (&optional count)
  "Decrease the fold-level of the visible area of the buffer. This unfolds
    another level of headings on each invocation."
  (interactive "p")
  (let ((new-level (+ (null/org--get-foldlevel) (or count 1))))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))

(defun null/org-close-all-folds (&optional level)
  "Close all folds in the buffer (or below LEVEL)."
  (interactive "p")
  (outline-hide-sublevels (or level 1)))

(defun null/org-open-all-folds (&optional level)
  "Open all folds in the buffer (or up to LEVEL)."
  (interactive "P")
  (if (integerp level)
      (outline-hide-sublevels level)

    (outline-show-all)))


(defun null/org-mode-setup ()
  (auto-fill-mode nil)
  (visual-line-mode 1)

  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.0))
  ;; (dolist (face '((org-level-1 . 1.0)
  ;;                 (org-level-2 . 1.0)
  ;;                 (org-level-3 . 1.0)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.0)
  ;;                 (org-level-6 . 1.0)
  ;;                 (org-level-7 . 1.0)
  ;;                 (org-level-8 . 1.0)))
  ;;   (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'bold :height (cdr face)))

  ;; ;; (require 'org-indent)
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-column nil :background nil)
  ;; (set-face-attribute 'org-column-title nil :background nil))

(use-package org
  :ensure t
  :hook
  (org-mode . null/org-mode-setup)
  (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-directory null/org-directory)
  (org-hide-leading-stars nil) ; superstar needs this to be nil
  (org-startup-indented t) ; indent mode on startup
  (org-indent-mode-turns-on-hiding-stars nil) ; superstar needs this to be nil

  ;; Visuals
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-truncated nil) ; Force org to not truncate lines
  (org-startup-with-inline-images "inlineimages")

  (org-imenu-depth 3) ; See more levels with imenu
  (org-return-follows-link t) ; use ret to follow link

  ;; Export settings
  (org-export-use-babel t) ; eval code on export
  (org-src-preserve-indentation t) ; keep leading whitespace in src blocks on export

   ;; Default apps
  (org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "zathura %s")
          ("\\.png\\'" . viewnior)
          ("\\.jpg\\'" . viewnior)
          ("\\.svg\\'" . viewnior)))

  :config
  ;; Tempo
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("as" . "aside"))
  (add-to-list 'org-structure-template-alist '("al" . "algorithm"))
  (add-to-list 'org-structure-template-alist '("pr" . "proof"))
  (add-to-list 'org-structure-template-alist '("th" . "theorem"))
  (add-to-list 'org-structure-template-alist '("cs" . "columns"))
  (add-to-list 'org-structure-template-alist '("co" . "column"))
  (add-to-list 'org-tempo-keywords-alist '("on" . "name"))
  (add-to-list 'org-tempo-keywords-alist '("oc" . "caption"))
  (add-to-list 'org-tempo-keywords-alist '("oo" . "attr_org"))
  (add-to-list 'org-tempo-keywords-alist '("ol" . "attr_latex")))

;; Pretty bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-headline-bullets-list '("◉" "◈" "○" "◇")))

;; Screenshotting
(defun org-download-named-screenshot (fname)
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
  (setq-default org-download-image-dir "../assets/images"
                org-download-heading-lvl nil))

;; Autotangle files marked #+auto_tangle: t
(use-package org-auto-tangle
  :after org
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))

;; Convenient evil bindings
(use-package evil-org
  :after '(org evil)
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)))

(null-keybinds-major-key-def
  :states 'normal
  :keymaps 'org-mode-map
  "a c" 'org-download-screenshot
  "a C" 'org-download-named-screenshot
  "k s" 'org-babel-demarcate-block
  "i l" 'org-cdlatex-environment-indent
  "i c" 'org-cite-insert
  "i f" 'org-footnote-new
  ;; "i p" 'academic-phrases
  "i s" 'org-insert-structure-template
  ;; "i P" 'academic-phrases-by-section
  "s n" 'org-toggle-narrow-to-subtree

  "t" 'org-todo
  "e" 'org-export-dispatch)

(general-define-key
 :states 'normal
 "z r" 'null/show-next-fold-level
 "z R" 'null/open-all-folds
 "z i" 'org-toggle-inline-images
 "z p" 'null/org-toggle-properties
 "C-n" 'org-babel-next-src-block
 "C-e" 'org-babel-previous-src-block)

(provide 'null-org)

;;; null-org.el ends here
