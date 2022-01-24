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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-collection fzf undo-fu evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(evil-define-key nil 'global (kbd "C-j") 'next-buffer)
(evil-define-key nil 'global (kbd "C-k") 'previous-buffer)
