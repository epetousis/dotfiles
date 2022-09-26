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
(global-set-key (kbd "C-x ,") 'fzf)

;; LSP
(use-package lsp-mode
  :init
  ;; Turn this horrible feature off so my life is good again
  (setq lsp-enable-on-type-formatting nil)
  :hook (
    (web-mode . lsp-deferred)
    (typescript-mode . lsp-deferred)
    (python-mode . lsp-deferred)
    (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
;; Set vscode-eslint server, but only if it exists
(setq eslint-server-location (car (last (file-expand-wildcards "~/.vscode/extensions/dbaeumer.vscode-eslint-*/server/out/eslintServer.js"))))
(if eslint-server-location (setq lsp-eslint-server-command `("node" ,(expand-file-name eslint-server-location) "--stdio")) nil)
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))
(use-package lsp-ui :after lsp-mode
  :commands lsp-ui-mode)
(use-package company :after lsp-mode)

;; Show available keybinds after a prefix keypress
(use-package which-key
  :config
  (which-key-mode))

;; Language specific major modes
(use-package typescript-mode :mode "\\.ts\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package web-mode
  :config (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode)))

;; Guess indentation automatically
(use-package dtrt-indent :config (dtrt-indent-global-mode))

;; Switch to dark mode automatically
(use-package auto-dark)

;; Add ability to use a project directory
(use-package projectile :config (projectile-mode +1))

;; Add ability to see errors without showing entire buffer
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Add modern syntax checking
;; Can be removed when/if https://github.com/emacs-lsp/lsp-mode/issues/2808 is fixed
(use-package flycheck
  :init (global-flycheck-mode))

;; Add a Git wrapper
(use-package magit)

;; Add editorconfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; UI Configuration
;; Hide window toolbar icons
(tool-bar-mode -1)
;; Make title bar blend into document on macOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Hide icon on macOS
(setq ns-use-proxy-icon nil)
;; Display line numbers
(global-display-line-numbers-mode)

;;; Keybinds
;; Ignore F18 (used for PTT)
(global-set-key (kbd "<f18>") 'ignore)
;; Unset easily pressable suspend key
(global-unset-key (kbd "C-z"))

;; Close FZF with esc (https://github.com/bling/fzf.el/issues/45#issuecomment-429893494)
(require 'term)
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))
;; to quit fzf with ESC key
(define-key term-raw-map (kbd "<escape>") 'term-send-esc)

;;; Extra configuration
;; direnv integration - allows us to easily use Nix packages
;; Place this late in the startup since minor modes prepend themselves to hooks
(use-package envrc
  :config
  (envrc-global-mode))
