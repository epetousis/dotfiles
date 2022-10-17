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
(use-package helm
  :bind ("C-x ," . helm-find))

;; LSP
(use-package eglot
  :config (add-to-list 'eglot-server-programs
                       '(web-mode . (eglot-volar "vue-language-server" "--stdio"))))
;; Using :hook causes an error - no idea why...
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
;; Always start Company when eglot runs
(add-hook 'eglot-managed-mode-hook 'company-mode)

(defclass eglot-volar (eglot-lsp-server) ()
  :documentation "A custom class for Volar's langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-volar))
  "Passes through required Volar initialization options"
  (let* ((tsdk (concat (projectile-project-root) "node_modules/typescript/lib")))
    (list :typescript (list :tsdk (file-name-as-directory tsdk)))))
(use-package company)

(defun flymake-eslint-enable-project ()
  " Ensure flymake-eslint uses our project-local eslint. "
  (setq flymake-eslint-executable-name (concat (projectile-project-root) "node_modules/.bin/eslint"))
  (flymake-eslint-enable))
(use-package flymake-eslint
  :after eglot
  :hook
  (web-mode . flymake-eslint-enable-project)
  (typescript-mode . flymake-eslint-enable-project))

;; Show available keybinds after a prefix keypress
(use-package which-key
  :config
  (which-key-mode))

;; Language specific major modes
(use-package typescript-mode :mode "\\.ts\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package web-mode
  :config (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode)))
(use-package rust-mode :mode "\\.rs\\'")

;; Guess indentation automatically
(use-package dtrt-indent :config (dtrt-indent-global-mode))

;; Add ability to use a project directory
(use-package projectile :config (projectile-mode +1))

;; Add ability to see errors without showing entire buffer
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Add a Git wrapper
(require 'magit-autoloads)

;; Add editorconfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; UI Configuration
;;; Theme
;; Add frame padding
(add-to-list 'default-frame-alist '(internal-border-width . 24))
;; Load monokai theme
(load-theme 'monokai t)
;; Hide scroll bars
(set-scroll-bar-mode nil)
;; Hide menubar
(menu-bar-mode -1)
;; Hide window toolbar icons
(tool-bar-mode -1)
;; Make title bar blend into document on macOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Hide icon on macOS
(setq ns-use-proxy-icon nil)
;; Display line numbers
(global-display-line-numbers-mode)
;; Display column numbers
(setq column-number-mode t)
;; Disable splash screen
(setq inhibit-splash-screen t)

;;; Keybinds
;; Ignore F18 (used for PTT)
(global-set-key (kbd "<f18>") 'ignore)
;; Unset easily pressable suspend key
(global-unset-key (kbd "C-z"))

;;; Custom functions
(defun web-mode-set-universal-padding (p)
  "Set a single padding value for all different web-mode padding options.
The argument P can be any number."
  (interactive "nSet padding to: ")
  (setq web-mode-block-padding p)
  (setq web-mode-part-padding p)
  (setq web-mode-script-padding p)
  (setq web-mode-style-padding p))

(defun nixos-rebuild (location)
  "Run a system nixos-rebuild from a particular flake.
The argument LOCATION can be any path to a Nix flake."
  (interactive "DWhere is the Nix flake? ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command (concat "nixos-rebuild switch --flake " (expand-file-name location)))))


;;; Extra configuration
;; direnv integration - allows us to easily use Nix packages
;; Place this late in the startup since minor modes prepend themselves to hooks
(use-package envrc
  :config
  (envrc-global-mode))
