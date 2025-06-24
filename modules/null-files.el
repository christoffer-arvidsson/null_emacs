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

(defun null/yank-buffer-path ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (let ((filename (abbreviate-file-name (if (equal major-mode 'dired-mode)
                                         default-directory
                                         (buffer-file-name)))))
    (when filename
      (let ((x-select-enable-clipboard t)) (kill-new filename))
      (message filename))))

(defun null/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's path relative to current projects root, to the kill ring."
  (interactive)
  (let ((filename (abbreviate-file-name (file-relative-name (if (equal major-mode 'dired-mode)
                                                             default-directory
                                                             (buffer-file-name)) (project-root (project-current))))))
    (when filename
      (let ((x-select-enable-clipboard t)) (kill-new filename))
      (message filename))))

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

(defun null/new-buffer (&optional force)
  "Edit a new unnamed buffer."
  (interactive)
  (let ((buffer (if force (generate-new-buffer "*new*")
                  (get-buffer-create "*new*"))))
    (set-buffer-major-mode buffer)
    (set-window-buffer nil buffer)))

(defun null/consult-ripgrep-current-directory ()
  "Search with `consult-ripgrep` from current directory."
  (interactive)
  (consult-ripgrep default-directory))

;; bookmarks
(null-keybinds-leader-key-def
  :states 'normal
  "RET" '(consult-bookmark :wk "Jump bookmark")

  "s i" '(consult-outline :wk "Consult outline")
  "s b" '(consult-line :wk "Search lines")
  "s s" '(null/search-symbol-at-point :wk "Search symbol at point")
  "s /" '(null/consult-ripgrep-current-directory :wk "Grep from current directory")

  "f r" '(consult-recent-file :wk "Recent files")
  "f m" '(chmod :wk "chmod")
  "f d" '(mkdir :wk "mkdir")
  "f f" '(find-file :wk "Find file")
  "f s" '(save-buffer :wk "Save buffer")
  "f p" '(null/find-config-file :wk "Find emacs config file")
  "f y" '(null/yank-buffer-path-relative-to-project :wk "Yank project-relative buffer path")
  "f Y" '(null/yank-buffer-path :wk "Yank buffer path")
  "f S" '(write-file :wk "Save buffer as...")
  "f D" '(null/delete-current-buffer-file :wk "Delete current file")
  "f R" '(null/rename-current-buffer-file :wk "Move current file")

  "b b" '(consult-buffer :wk "Switch buffer")
  "b s" '(save-buffer :wk "Save buffer")
  "b r" '(revert-buffer :wk "Revert buffer")
  "b d" '(kill-current-buffer :wk "Kill current buffer")
  "b n" '(null/new-buffer :wk "Edit new unnamed buffer")
  "b N" '(lambda () (interactive)(null/new-buffer t) :wk "Create new unnamed buffer"))

(provide 'null-files)
