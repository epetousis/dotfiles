(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(package-refresh-contents t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(defun set-exec-path-from-shell-PATH ()
  ;; Taken from https://www.emacswiki.org/emacs/ExecPath
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; Install packages
(use-package fzf)

;; LSP
(use-package lsp-mode
  :hook (
    (vue-mode . lsp-deferred)
    (typescript-mode . lsp))
  :commands (lsp lsp-deferred))
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
(use-package lsp-ui :after lsp-mode
  :commands lsp-ui-mode)
(use-package company :after lsp-mode)

;; Evil mode related settings
; Enable evil mode
;; `evil-collection' assumes `evil-want-keybinding' is set to
;; `nil' before loading `evil' and `evil-collection'
;; @see https://github.com/emacs-evil/evil-collection#installation
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  ;; Change evil's initial state in term mode to emacs (zsh should be in vi-mode anyway)
  (evil-set-initial-state 'term-mode 'emacs)
  ;; Define keybinds in evil
  (evil-define-key nil 'global (kbd "C-j") 'next-buffer)
  (evil-define-key nil 'global (kbd "C-k") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "g h") 'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd ", z") 'fzf))
(use-package evil-collection
  :after evil
  :config
  ;; Register evil-collection bindings
  (evil-collection-init))

(use-package which-key
  :config
  (which-key-mode))

;; Close FZF with esc (https://github.com/bling/fzf.el/issues/45#issuecomment-429893494)
(require 'term)
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))
;; to quit fzf with ESC key
(define-key term-raw-map (kbd "<escape>") 'term-send-esc)

;; Move all autosaves to a directory
(setq autosave-dir "~/.emacs-saves/")
(unless (file-exists-p autosave-dir)
  (make-directory autosave-dir t))
(setq auto-save-file-name-transforms
  `((".*" ,autosave-dir t)))

;; Language specific major modes
(use-package vue-mode :mode "\\.vue\\'"
  ;; Disable that fugly mmm-mode background default
  :init
  (setq mmm-submode-decoration-level 0))
(use-package typescript-mode :mode "\\.ts\\'")

;; Guess indentation automatically
(use-package dtrt-indent :config (setq dtrt-indent-global-mode t))

;; direnv integration - allows us to easily use Nix packages
;; Place this late in the startup since minor modes prepend themselves to hooks
(use-package envrc
  :config
  (envrc-global-mode))
