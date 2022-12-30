;;; null-rust.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rustic
  :ensure t)

(null-keybinds-major-key-def
  :keymaps 'normal
  "b r" 'rustic-cargo-run
  "b b" 'rustic-cargo-build
  "b c" 'rustic-compile
  "b C" 'rustic-cargo-clippy
  "b f" 'rustic-format-buffer
  "t t" 'rustic-cargo-current-test
  "t a" 'rustic-cargo-test
  "t B" 'rustic-cargo-bench
  "c a" 'rustic-cargo-add
  "c d" 'rustic-cargo-add
  "c n" 'rustic-cargo-new
  "c u" 'rustic-cargo-update
  "c U" 'rustic-cargo-upgrade)

(provide 'null-rust)

;;; null-rust.el ends here
