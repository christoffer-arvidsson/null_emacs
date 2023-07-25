;;; null-org-html.el --- html export for org -*- lexical-binding: t -*-

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

;;; Code:

(with-eval-after-load 'org
  (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"/home/eethern/Dropbox/org/orbit/assets/style.css\" type=\"text/css\"/>"
        org-html-postamble nil
        org-html-head-include-scripts nil
        org-html-with-latex 'mathjax))

(provide 'null-org-html)

;;; null-org-html.el ends here
