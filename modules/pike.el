;;; pike.el --- Bookmarking utilities -*- lexical-binding: t -*-

;; Author: Christoffer Arvidsson
;; Maintainer: Christoffer Arvidsson
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: tools languages


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

;; Todos
;; - [x] separate by repo
;;   - [x] separate by branch
;; - [ ] make project package selectable
;; - [ ] make binding global vs non-global easier

;;; Code:

(require 'project)

(defgroup pike nil
  "Quick-jump organization."
  :group 'tools)

(defcustom pike-cache-directory (expand-file-name user-emacs-directory ".local/pike")
  "Where the cache files will be saved."
  :type 'string
  :group 'pike)

(defun pike--create-cache-directory ()
  "Create cache directory of pike if it doesn't exist."
  (unless (file-directory-p pike-cache-directory)
    (make-directory pike-cache-directory)))

(defun pike--create-cache-file (cache-file)
  "Create CACHE-FILE if it doesn't exist."
  (unless (file-exists-p cache-file)
    (with-temp-buffer (write-file cache-file))))

(defun pike--global-cache-file-path ()
  "Get the file path for the global cache."
  (expand-file-name "pike_global" pike-cache-directory))

(defun pike--project-branch ()
  "Get the project git branch name."
  (let ((root (project-root (project-current))))
    (car (split-string
          (shell-command-to-string
           (concat "cd " root "; git rev-parse --abbrev-ref HEAD")) "\n"))))

(defun pike--project-cache-file-path ()
  "Get the file path for the current project cache."
  (if-let ((project (project-current))
           (root (project-root project))
           (branch (pike--project-branch)))
      (format "%s--%s"
              (expand-file-name (file-name-nondirectory
                                 (directory-file-name root))
                                pike-cache-directory)
              branch)
    (message "Could not determine project or branch.")))

(defun pike--cache-file-path (&optional global)
  "Get the cache file path for the current project.
If GLOBAL is non-nil then get the global cache file path."
  (if global
      (pike--global-cache-file-path)
    (pike--project-cache-file-path)))

(defun pike--revert-cache-buffer (cache-file)
  "Revert the CACHE-FILE buffer."
  (when-let (buffer (get-buffer cache-file))
    (with-current-buffer buffer
      (revert-buffer nil t))))

(defun pike--get-buffer (&optional global)
  "Get pike buffer.
If GLOBAL is non-nil then get the global pike buffer."
  (find-file-noselect (pike--cache-file-path global)))

(defun pike--num-entries (cache-file)
  "Get number of entries in CACHE-FILE."
  (with-temp-buffer
    (insert-file-contents cache-file)
    (count-lines (point-min) (point-max))))

(defun pike--find-cache-number (cache-file search-string)
  "Get the line number of entry in CACHE-FILE matching SEARCH-STRING, or nil."
  (with-temp-buffer
    (insert-file-contents cache-file)
    (goto-char (point-min))
    (when (search-forward-regexp (regexp-quote search-string) nil t)
      (line-number-at-pos (line-beginning-position)))))

(defun pike--get-cache-key-at-line (cache-file line-number)
  "Get the line from CACHE-FILE at the specified LINE-NUMBER."
  (with-temp-buffer
    (insert-file-contents cache-file)
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((row (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (if (or (= (length row) 0)
              (not (eq (line-number-at-pos (point)) line-number)))
          nil row))))

;;;###autoload
(defun pike-clear (&optional global)
  "Clear the global pike files.
If GLOBAL is non-nil then clear the global cache file."
  (interactive)
  (pike--create-cache-directory)
  (with-current-buffer (pike--get-buffer global)
    (progn (erase-buffer)
           (message "Pike buffer cleared.")
           (flush-lines "^$")
           (save-buffer))))

(defun pike--find (cache-file line-number)
  "Find file at LINE-NUMBER in CACHE-FILE."
  (pike--create-cache-directory)
  (pike--create-cache-file cache-file)
  (if-let ((cache-key (pike--get-cache-key-at-line cache-file line-number)))
      (if (get-buffer cache-key)
          (switch-to-buffer cache-key)
        (find-file cache-key))
    (message (format "Could not find pike file in entry %d" line-number))))

(defun pike--get-cache-key ()
  "Get the cache key of the current buffer."
  (let ((filename (buffer-file-name)))
    ;; is it a file or directory
    (if filename
        (abbreviate-file-name
         (if (equal major-mode 'dired-mode)
             default-directory filename))
      (if (buffer-name)
          (buffer-name)
        (message "Not a valid buffer to store in pike.")))))

;;;###autoload
(defun pike-add-key (&optional global)
  "Add current key to pike.
If GLOBAL is non-nil then add to global cache."
  (interactive)
  (pike--create-cache-directory)
  (let ((cache-file (pike--cache-file-path global))
        (cache-key (pike--get-cache-key)))
    (pike--create-cache-file cache-file)
    (if (pike--find-cache-number cache-file cache-key)
        (message (format "Key %s already in pike." cache-key))
      (with-current-buffer (pike--get-buffer global)
        (goto-char (point-max))
        (insert cache-key)
        (insert hard-newline)
        (save-buffer)))
    (pike--revert-cache-buffer cache-file)))

;;;###autoload
  (defun pike-open-buffer (&optional global)
    "Open the pike cache buffer.
If GLOBAL is non-nil open the global buffer."
    ;; TODO: We want to be able to open the global buffer from the
    ;; non-global ones, and vice-versa. Just checking the mode is not
    ;; enough here.
    (interactive)
    (unless (eq major-mode 'pike-mode)
      (pike--create-cache-directory)
      (let ((buffer (pike--get-buffer global)))
        (with-current-buffer buffer
          (pike-mode))
        (display-buffer buffer))))

;;;###autoload
  (defun pike-find-1 ()
    "Find pike file 1."
    (interactive)
    (pike--find (pike--project-cache-file-path) 1))

;;;###autoload
  (defun pike-find-2 ()
    "Find pike file 2."
    (interactive)
    (pike--find (pike--project-cache-file-path) 2))

;;;###autoload
  (defun pike-find-3 ()
    "Find pike file 3."
    (interactive)
    (pike--find (pike--project-cache-file-path) 3))

;;;###autoload
  (defun pike-find-4 ()
    "Find pike file 4."
    (interactive)
    (pike--find (pike--project-cache-file-path) 4))

;;;###autoload
  (defun pike-find-global-1 ()
    "Find global pike file 1."
    (interactive)
    (pike--find (pike--global-cache-file-path) 1))

;;;###autoload
  (defun pike-find-global-2 ()
    "Find global pike file 2."
    (interactive)
    (pike--find (pike--global-cache-file-path) 2))

;;;###autoload
  (defun pike-find-global-3 ()
    "Find global pike file 3."
    (interactive)
    (pike--find (pike--global-cache-file-path) 3))

;;;###autoload
  (defun pike-find-global-4 ()
    "Find global pike file 4."
    (interactive)
    (pike--find (pike--global-cache-file-path) 4))

;;;###autoload
  (defun pike-next (&optional global)
    "Find the next file in the pike file ring.
Loops around if at the end.
If GLOBAL is non-nil then use the global pike cache."
    (interactive)
    (let* ((cache-file (pike--cache-file-path global))
           (num-lines (pike--num-entries cache-file))
           (line-number (pike--find-cache-number cache-file
                                                 (pike--get-cache-key))))
      (if line-number
          (pike--find cache-file (1+ (mod line-number num-lines)))
        (unless (= num-lines 0)
          (pike--find cache-file 1)))))

;;;###autoload
  (defun pike-previous (&optional global)
    "Find the previous file in the pike file ring.
Loops around if at the beginning.
If GLOBAL is non-nil then use the global pike cache."
    (interactive)
    (let* ((cache-file (pike--cache-file-path global))
           (num-lines (pike--num-entries cache-file))
           (line-number (pike--find-cache-number cache-file
                                                 (pike--get-cache-key))))
      (if line-number
          (pike--find cache-file (1+ (mod (- line-number 2) num-lines)))
        (unless (= num-lines 0)
          (pike--find cache-file 1)))))

  (define-derived-mode pike-mode nil "Pike"
    "Mode for pike buffer."
    (display-line-numbers-mode t))

  (provide 'pike)
;;; pike.el ends here
