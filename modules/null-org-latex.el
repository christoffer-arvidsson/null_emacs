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

;; (defun null/setup-minted ()
;;   (setq org-latex-listings 'minted
;;         org-latex-custom-lang-environments '((emacs-lisp "common-lispcode"))
;;         org-latex-minted-options
;;         '(("bgcolor" "bgcode")
;;           ("fontsize" "\\scriptsize")
;;           ("baselinestretch" "0.9")
;;           ("framesep" "3mm")
;;           ("breaklines" "true")
;;           ("linenos" "true")
;;           ("numbersep" "2mm")
;;           ("xleftmargin" "6mm"))))

;; Breaks org-latex-preview if not loader here
(use-package citeproc
  :after org)

(with-eval-after-load 'org
  ;; (null/setup-minted)
  (setq org-latex-default-class "orbit"
        ;; org-latex-packages-alist '(("" "minted"))
        org-cite-export-processors '((t csl))
        org-latex-tables-booktabs t
        org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"cita))


  (setq org-latex-packages-alist
        '(("T1" "fontenc" t)
          ("" "amsmath" t)
          ("" "bm" t) ; Bold math required
          ("" "mathtools" t)
          ("" "siunitx" t)
          ("" "algpseudocode" t)
          ("" "algorithm" t))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("orbit"
                 "\\documentclass[a4paper,11pt]{article}
\\usepackage[a4paper, margin=3cm]{geometry}
\\usepackage[dvipsnames]{xcolor}
\\definecolor{bgcode}{rgb}{0.95,0.95,0.95}

\\usepackage{amsmath}
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

(use-package org-latex-preview
  :after (org citeproc)
  :ensure nil
  :config
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)
  (setq org-latex-preview-process-default 'dvisvgm
        org-latex-preview-live-debounce 0.25
        org-latex-preview-numbered t
        org-latex-preview-live t
        org-startup-with-latex-preview t))


(use-package cdlatex
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  :hook (org-mode . org-cdlatex-mode)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(provide 'null-org-latex)

;;; null-org-latex.el ends here
