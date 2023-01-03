;;; null-files.el -*- lexical-binding: t; -*-

;;; Code:

(require 'null-keybinds)

(defun null/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun null/rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun null/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (message "Copied path to clipboard: %s"
               (kill-new (abbreviate-file-name
                          (if root
                              (file-relative-name filename root)
                            filename))))
    (error "Couldn't find filename in current buffer")))

(defun null/yank-buffer-path-relative-to-project (&optional include-root)
  "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
  (interactive "P")
  (null/yank-buffer-path
   (if include-root
       (file-name-directory (directory-file-name (project-root)))
     (project-root))))

(defun null/find-project-file-in-directory (directory)
  "Find project file in DIRECTORY."
  (let ((default-directory directory))
    (project-find-file)))

(defun null/find-config-file ()
  "Find project file in Emacs user directory."
  (interactive)
  (null/find-project-file-in-directory user-emacs-directory))

(defun null/search-symbol-at-point ()
  "Search lines by symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;; bookmarks
(null-keybinds-leader-key-def
  :keymaps 'normal
  "RET" '(consult-bookmark :wk "Jump bookmark")

  "s" '(:ignore t :wk "search")
  "s i" '(consult-imenu :wk "Jump to symbol")
  "s b" '(consult-line :wk "Search lines")
  "s s" '(null/search-symbol-at-point :wk "Search symbol at point")

  "f" '(:ignore t :wk "file")
  "f r" '(consult-recent-file :wk "Recent files")
  "f f" '(find-file :wk "Find file")
  "f s" '(save-buffer :wk "Save buffer")
  "f p" '(null/find-config-file :wk "Find emacs config file")
  "f y" '(null/yank-buffer-path :wk "Yank buffer path")
  "f Y" '(null/yank-buffer-path-relative-to-project :wk "Yank project-relative buffer path")
  "f S" '(write-file :wk "Save buffer as...")
  "f D" '(null/delete-current-buffer-file :wk "Delete current file")
  "f R" '(null/rename-current-buffer-file :wk "Move current file")

  "b" '(:ignore t :wk "buffer")
  "b b" '(consult-buffer :wk "Switch buffer")
  "b s" '(save-buffer :wk "Save buffer")
  "b r" '(revert-buffer :wk "Revert buffer")
  "b d" '(kill-this-buffer :wk "Kill current buffer")
  ;; "o r" '(ranger :wk "Open ranger")
  ;; "o ." '(dirvish :wk "Open dirvish")
  )

(provide 'null-files)
