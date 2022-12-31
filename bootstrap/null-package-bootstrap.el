(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(defun null-package-initialize ()
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(provide 'null-package/package)
