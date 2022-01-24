(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Download undo-fu (remove with Emacs 28)
(package-install 'undo-fu)

;; Install packages
(unless (package-installed-p 'fzf)
  (package-install 'fzf))
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))

;; Evil mode related settings
(setq evil-undo-system 'undo-fu)
;; `evil-collection' assumes `evil-want-keybinding' is set to
;; `nil' before loading `evil' and `evil-collection'
;; @see https://github.com/emacs-evil/evil-collection#installation
(setq evil-want-keybinding nil)
;; Enable Evil mode
(require 'evil)
(evil-mode 1)

;; Register evil-collection bindings
(evil-collection-init)

;; Close FZF with esc (https://github.com/bling/fzf.el/issues/45#issuecomment-429893494)
(require 'term)
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))
;; to quit fzf with ESC key
(define-key term-raw-map (kbd "<escape>") 'term-send-esc)

;; Change evil's initial state in term mode to emacs (zsh should be in vi-mode anyway)
(evil-set-initial-state 'term-mode 'emacs)

(evil-define-key nil 'global (kbd "C-j") 'next-buffer)
(evil-define-key nil 'global (kbd "C-k") 'previous-buffer)

;; Move all autosaves to a directory
(setq autosave-dir "~/.emacs-saves/")
(unless (file-exists-p autosave-dir)
  (make-directory autosave-dir t))
(setq auto-save-file-name-transforms
  `((".*" ,autosave-dir t)))

;; Move custom vars to separate file - create if needed
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
