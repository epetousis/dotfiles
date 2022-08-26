(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(package-refresh-contents t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; Install packages
(use-package fzf :ensure t)

;; LSP
(use-package lsp-mode :ensure t
  :hook (
    (vue-mode . lsp-deferred)
    (typescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :after lsp-mode :ensure t
  :commands lsp-ui-mode)
(use-package company :after lsp-mode :ensure t)

;; Evil mode related settings
; Enable evil mode
;; `evil-collection' assumes `evil-want-keybinding' is set to
;; `nil' before loading `evil' and `evil-collection'
;; @see https://github.com/emacs-evil/evil-collection#installation
(use-package evil
  :ensure t
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
  (define-key evil-normal-state-map (kbd "g h") 'lsp-ui-doc-glance))
(use-package evil-collection :ensure t
  :after evil
  :config
  ;; Register evil-collection bindings
  (evil-collection-init))

(use-package which-key :ensure t
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
(use-package vue-mode :ensure t :mode "\\.vue\\'"
  ;; Disable that fugly mmm-mode background default
  :init
  (setq mmm-submode-decoration-level 0))
(use-package typescript-mode :ensure t :mode "\\.ts\\'")

;; direnv integration - allows us to easily use Nix packages
;; Place this late in the startup since minor modes prepend themselves to hooks
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))
