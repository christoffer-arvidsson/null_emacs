;;; null-org-latex.el --- summary -*- lexical-binding: t -*-

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

;; (defun my-latex-export-example-blocks (text backend info)
;;   "Export example blocks as listings env."
;;   (when (org-export-derived-backend-p backend 'latex)
;;     (with-temp-buffer
;;       (insert text)
;;       ;; replace verbatim env by listings
;;       (goto-char (point-min))
;;       (replace-string "\\begin{verbatim}" "\\begin{lstlisting}")
;;       (replace-string "\\end{verbatim}" "\\end{lstlisting}")
;;       (buffer-substring-no-properties (point-min) (point-max)))))

;; (add-to-list 'org-export-filter-example-block-functions
;;          'my-latex-export-example-blocks)

(defun null/setup-minted ()
  (setq org-latex-listings 'minted
        org-latex-custom-lang-environments '((emacs-lisp "common-lispcode"))
        org-latex-minted-options
        '(("bgcolor" "bgcode")
          ("fontsize" "\\scriptsize")
          ("baselinestretch" "0.9")
          ("framesep" "3mm")
          ("breaklines" "true")
          ("linenos" "true")
          ("numbersep" "2mm")
          ("xleftmargin" "6mm"))))

;; Scale up latex fragments
(with-eval-after-load 'org
  (null/setup-minted)
  (setq org-latex-default-class "orbit"
        org-latex-packages-alist '(("" "minted"))
        org-cite-export-processors '((t csl))
        org-latex-tables-booktabs t
        org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

  (plist-put org-format-latex-options :scale 1.0))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("orbit"
                 "\\documentclass[a4paper,11pt]{article}
\\usepackage[a4paper, margin=3cm]{geometry}
\\usepackage[dvipsnames]{xcolor}
\\definecolor{bgcode}{rgb}{0.95,0.95,0.95}

\\usepackage{booktabs}
\\usepackage{gentium}
\\usepackage{fancyhdr}
\\usepackage{microtype}
\\usepackage{mathtools}
\\usepackage{optidef}
\\usepackage{bm}
\\usepackage[ruled,vlined]{algorithm2e}
\\usepackage{esvect}
\\usepackage{esdiff}
\\usepackage[parfill]{parskip}
\\usepackage{pgf}
\\usepackage{minted}
\\usemintedstyle{trac}
\\usepackage{fancyvrb}
\\usepackage{fvextra}

\\usepackage{etoolbox}
\\AtBeginEnvironment{tabular}{\\scriptsize}

\\DefineVerbatimEnvironment{wideverbatim}{Verbatim}{%
  gobble=0,
  numbers=left,
  numbersep=1mm,
  fontsize=\\scriptsize,
  rulecolor=\color{gray},
  xleftmargin=10mm,
  breaklines=true,
}

\\RecustomVerbatimEnvironment{verbatim}{Verbatim}{%
  gobble=0,
  fontsize=\\scriptsize,
  rulecolor=\color{gray},
  xleftmargin=5mm,
  breaklines=true,
  breakanywhere=true,
}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Latex
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-fragtog-ignore-predicates '(org-at-table-p)))

(setq org-preview-latex-default-process 'dvisvgm) ;No blur when scaling

(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

(use-package cdlatex
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  :hook (org-mode . org-cdlatex-mode)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

;; Numbered equations all have (1) as the number for fragments with vanilla
;; org-mode. This code injects the correct numbers into the previews so they
;; look good.
(defun scimax-org-renumber-environment (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
        (counter -1)
        (numberp))
    (setq results (cl-loop for (begin . env) in
                           (org-element-map (org-element-parse-buffer) 'latex-environment
                             (lambda (env)
                               (cons
                                (org-element-property :begin env)
                                (org-element-property :value env))))
                           collect
                           (cond
                            ((and (string-match "\\\\begin{equation}" env)
                                  (not (string-match "\\\\tag{" env)))
                             (cl-incf counter)
                             (cons begin counter))
                            ((string-match "\\\\begin{align}" env)
                             (prog2
                                 (cl-incf counter)
                                 (cons begin counter)
                               (with-temp-buffer
                                 (insert env)
                                 (goto-char (point-min))
                                 ;; \\ is used for a new line. Each one leads to a number
                                 (cl-incf counter (count-matches "\\\\$"))
                                 ;; unless there are nonumbers.
                                 (goto-char (point-min))
                                 (cl-decf counter (count-matches "\\nonumber")))))
                            (t
                             (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))


(defun scimax-toggle-latex-equation-numbering ()
  "Toggle whether LaTeX fragments are numbered."
  (interactive)
  (if (not (get 'scimax-org-renumber-environment 'enabled))
      (progn
        (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
        (put 'scimax-org-renumber-environment 'enabled t)
        (message "Latex numbering enabled"))
    (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
    (put 'scimax-org-renumber-environment 'enabled nil)
    (message "Latex numbering disabled.")))

(advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
(put 'scimax-org-renumber-environment 'enabled t)

(provide 'null-org-latex)

;;; null-org-latex.el ends here
