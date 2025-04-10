;;; null-font.el --- Font config -*- lexical-binding: t -*-

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

;; Just a place to define fonts.

;;; Code:

(require 'null-keybinds)

(defvar null-font-preset 'default
  "Fontaine font preset to use.")

(defvar null-font-big-preset 'big
  "Fontaine big font preset to use.")

(use-package fontaine
  :custom
  (fontaine-presets
   '(
     (default
      :default-family "Monospace")
     (desktop
      :default-family "Iosevka Nerd Font"
      :default-weight regular
      :default-height 110
      :variable-pitch-family "Iosevka"
      :variable-pitch-weight nil
      :variable-pitch-height 1.00)
     (big
      :inherit desktop
      :default-height 150)
     (laptop
      :inherit desktop
      :default-family "Iosevka Nerd Font"
      :variable-pitch-family "Iosevka"
      :default-height 95)
     (laptop-big
      :inherit laptop
      :default-height 140)
     (t
      :bold weight bold
      :italic-slant italic
      :line-spacing nil)))
  :config
  (define-minor-mode null-global-big-text-mode
    "Toggle big text mode."
    :init-value nil
    :global t
    :group 'null
    :lighter " big-text"
    (if null-global-big-text-mode
        (progn
          (message "big-text-mode activated!")
          (fontaine-set-preset null-font-big-preset))
      (progn (message "big-text-mode deactivated!")
             (fontaine-set-preset null-font-preset))))
  (fontaine-mode +1))

(null-keybinds-leader-key-def
  :states 'normal
  "t b" '(lambda () (interactive) (null-global-big-text-mode 'toggle)))


(provide 'null-font)

;;; null-font.el ends here
