;;; null-docker.el --- Docker module -*- lexical-binding: t -*-

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

;; Some useful docker packages!

;;; Code:

(use-package dockerfile-mode
  )

(use-package docker-compose-mode
  )

(use-package docker
  )

(defun null/capitalize-dockerfile-keywords ()
  "Capitalize Dockerfile keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(FROM\\|RUN\\|CMD\\|ENTRYPOINT\\|EXPOSE\\|ENV\\|ADD\\|COPY\\|WORKDIR\\|VOLUME\\|USER\\|ARG\\|LABEL\\|MAINTAINER\\|STOPSIGNAL\\|HEALTHCHECK\\|SHELL\\)" nil t)
      (replace-match (upcase (match-string 0))))))

(defun null/capitalize-dockerfile-keywords-on-save ()
  "Run null/capitalize-dockerfile-keywords function on save."
  (when (or (eq major-mode 'dockerfile-mode)
            (eq major-mode 'dockerfile-ts-mode))
    (null/capitalize-dockerfile-keywords)))

(add-hook 'before-save-hook 'null/capitalize-dockerfile-keywords-on-save)

(null-keybinds-leader-key-def
  :states 'normal
  "o d" '(docker :wk "Open docker"))

(provide 'null-docker)

;;; null-docker.el ends here
