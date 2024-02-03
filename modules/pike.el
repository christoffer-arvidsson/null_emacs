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

;; commentary

;;; Code:

(defgroup pike nil
  "Quick-jump organization."
  :group 'tools)

(defcustom pike-cache-directory (expand-file-name user-emacs-directory ".local/pike")
  "Where the cache files will be saved."
  :type 'str
  :group 'pike)

(defun pike--create-cache-directory ()
  "Create global cache directory of pike if it doesn't exist."
  (unless (file-directory-p pike-cache-directory)
    (make-directory pike-cache-directory)))

(defun pike--global-cache-file-name ()
  "Get the global cache filename."
  (expand-file-name "pike_global" pike-cache-directory))

(defun pike--revert-cache-buffer ()
  "Revert the cache buffer."
  (when-let (buffer (get-buffer
                     (pike--global-cache-file-name)))
      (with-current-buffer buffer
        (revert-buffer nil t))))

(defun pike--get-buffer ()
  "Get pike buffer."
  (find-file-noselect (pike--global-cache-file-name)))

(defun pike--num-entries ()
  "Get number of entries in cache buffer."
  (with-temp-buffer
    (insert-file-contents (pike--global-cache-file-name))
    (count-lines (point-min) (point-max))))

(defun pike--find-file-number (search-string)
  "Get the line number of entry in cache matching SEARCH-STRING, or nil."
  (with-temp-buffer
    (insert-file-contents (pike--global-cache-file-name))
    (goto-char (point-min))
    (when (search-forward-regexp (format "^%s$" search-string) nil t)
        (line-number-at-pos (line-beginning-position)))))

(defun pike--get-file (line-number)
  "Get the line from cache at the specified LINE-NUMBER."
  (with-temp-buffer
    (insert-file-contents (pike--global-cache-file-name))
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((row (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (message row)
      (if (or (= (length row) 0)
              (not (eq (line-number-at-pos (point)) line-number)))
          nil row))))

;;;###autoload
(defun pike-clear ()
  "Clear the global pike files."
  (interactive)
  (pike--create-cache-directory)
  (with-current-buffer (pike--get-buffer)
    (progn (erase-buffer)
           (message "Pike global buffer cleared.")
           (flush-lines "^$")
           (save-buffer))))

(defun pike--find (line-number)
  "Find file at LINE-NUMBER."
  (let ((filename (pike--get-file line-number)))
    (if filename
        (find-file filename)
      (message (format "Could not find pike file in entry %d" line-number)))))

(defun pike--save-and-quit-window ()
  "Save and quit pike buffer."
  (when (eq major-mode 'pike-mode)
    (flush-lines "^$")
    (save-buffer)
    (kill-buffer-and-window)))

(defun pike--get-cache-key ()
  "Get the cache key of the current buffer."
  (abbreviate-file-name
   (if (equal major-mode 'dired-mode)
       default-directory (buffer-file-name))))

;;;###autoload
(defun pike-add-file ()
  "Add current file to pike."
  (interactive)
  (pike--create-cache-directory)
  (let ((filename (pike--get-cache-key)))
    (if (pike--find-file-number filename)
        (message (format "File %s already in pike." filename))
      (with-current-buffer (pike--get-buffer)
        (goto-char (point-max))
        (insert filename)
        (insert hard-newline)
        (save-buffer))))
  (pike--revert-cache-buffer))

;;;###autoload
  (defun pike-open-buffer ()
    "Open the pike cache buffer."
    (interactive)
    (unless (eq major-mode 'pike-mode)
      (pike--create-cache-directory)
      (let ((buffer-name "pike_global")
            (buffer (pike--get-buffer)))
        (with-current-buffer buffer
          (pike-mode)
          (rename-buffer buffer-name))
        (display-buffer buffer))))

;;;###autoload
(defun pike-find-1 ()
  "Find pike file 1."
  (interactive)
  (pike--find 1))

;;;###autoload
(defun pike-find-2 ()
  "Find pike file 2."
  (interactive)
  (pike--find 2))

;;;###autoload
(defun pike-find-3 ()
  "Find pike file 3."
  (interactive)
  (pike--find 3))

;;;###autoload
(defun pike-find-4 ()
  "Find pike file 4."
  (interactive)
  (pike--find 4))

;;;###autoload
(defun pike-next ()
  "Find the next file in the pike file ring.
Loops around if at the end."
  (interactive)
  (let* ((num-lines (pike--num-entries))
         (line-number (pike--find-file-number (pike--get-cache-key))))
    (if line-number
      (pike--find (1+ (mod line-number num-lines)))
      (message "`pike-next` only works in files stored in pike."))))

;;;###autoload
(defun pike-previous ()
  "Find the previous file in the pike file ring.
Loops around if at the beginning."
  (interactive)
  (let* ((num-lines (pike--num-entries))
         (line-number (pike--find-file-number (pike--get-cache-key))))
    (if line-number
        (pike--find (1+ (mod (- line-number 2) num-lines)))
      (message "`pike-previous` only works in files stored in pike."))))

(define-derived-mode pike-mode nil "Pike"
  "Mode for pike buffer."
  (display-line-numbers-mode t))

(provide 'pike)
;;; pike.el ends here
