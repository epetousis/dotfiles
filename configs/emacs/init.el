;; Set exec path for macOS's sake
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

;; Make sure that emacs runs as a server.
(server-start)

;;;; LSP

;;; Set up eglot
(require 'eglot)
(add-to-list 'eglot-server-programs
             '(vue-mode . (eglot-volar "vue-language-server" "--stdio"))
             '(nix-mode . ("nil")))

(add-hook 'vue-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)

;; Override editorconfig when in vue-mode - seems to be wrong about padding settings
(add-hook 'editorconfig-after-apply-functions (lambda (props)
                                                (when (derived-mode-p 'vue-mode)
                                                  (web-mode-set-universal-padding 0))))
(add-hook 'typescript-mode-hook 'eglot-ensure)

(defclass eglot-volar (eglot-lsp-server) ()
  :documentation "A custom class for Volar's langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-volar))
  "Passes through required Volar initialization options"
  (let* ((tsdk (concat (current-project-root) "node_modules/typescript/lib")))
    (list :typescript (list :tsdk tsdk)
          :vue (list :hybridMode :json-false))))

;; Language specific major modes
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
;; Define a custom mode just for Vue, so that eglot sends the correct languageId.
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Guess indentation automatically
(require 'dtrt-indent)
(dtrt-indent-global-mode)

;; Add editorconfig support
(require 'editorconfig)
(editorconfig-mode 1)

;; Enable a global project breadcrumb header line
(require 'breadcrumb)
(breadcrumb-mode)

;;; UI Configuration
;;; Theme
;; Add ef-duo-dark
(require 'ef-themes)
(load-theme 'ef-duo-dark t)
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
;; Display column numbers in modeline
(setq column-number-mode t)
;; Disable splash screen
(setq inhibit-splash-screen t)
;; Enable icomplete-mode everywhere
(icomplete-mode 1)
;; Enable completion-preview-mode
(global-completion-preview-mode)
;; Enable native full screen
(add-to-list 'default-frame-alist '(ns-use-native-fullscreen . t))
;; Disable title bar
(add-to-list 'default-frame-alist '(undecorated-round . t))

(require 'dired-sidebar)

(require 'dired-preview)
(dired-preview-global-mode 1)

;;; Keybinds
;; Ignore F18 (used for PTT)
(global-set-key (kbd "<f18>") 'ignore)
;; Unset easily pressable suspend key
(global-unset-key (kbd "C-z"))
;; Fix display-local-help being bound to eldoc by eglot
;; Taken from https://github.com/joaotavora/eglot/issues/454#issuecomment-642978840
(define-key eglot-mode-map [remap display-local-help] nil)
(define-key eglot-mode-map (kbd "C-c /") #'eglot-code-actions)
;; Define an alternative key for eldoc
(global-set-key (kbd "C-h ,") #'eldoc-doc-buffer)
;; Define a key to allow sending bound keys in vterm (for example, sending C-x to nano)

(require 'vterm)
(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)

;; Taken from https://gist.github.com/ethan-leba/760054f36a2f7c144c6b06ab6458fae6
(defun wm-move-on-error (direction move-fn)
  (interactive)
  (condition-case nil
      (funcall move-fn)
    (user-error (condition-case nil
                    ;; Try shifting focus in a stack context first - if it fails, use normal directions
                    (start-process "yabai" nil "yabai" "-m" "window" "--focus" (pcase direction
                                                                                     ("south" "stack.next")
                                                                                     ("north" "stack.prev")
                                                                                     (_ 'direction)))
                  (error (condition-case nil
                             (start-process "yabai" nil "yabai" "-m" "window" "--focus" direction)
                           (error nil)))))))

(defun wm-window-left ()
  (interactive)
  (wm-move-on-error "west" #'windmove-left))

(defun wm-window-right ()
  (interactive)
  (wm-move-on-error "east" #'windmove-right))

(defun wm-window-up ()
  (interactive)
  (wm-move-on-error "north" #'windmove-up))

(defun wm-window-down ()
  (interactive)
  (wm-move-on-error "south" #'windmove-down))

;; Duplicate window move binds from tiling window manager
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(setq x-super-keysym 'meta)
(setq x-meta-keysym 'alt)
(global-set-key (kbd "A-h") #'wm-window-left)
(global-set-key (kbd "A-j") #'wm-window-down)
(global-set-key (kbd "A-k") #'wm-window-up)
(global-set-key (kbd "A-l") #'wm-window-right)

;; org-mode bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Add a keybind to quickly show a dired sidebar
(keymap-global-set "C-c ;" #'dired-sidebar-toggle-sidebar)

;; Shortcuts to make markdown-mode more like org-mode
(require 'markdown-mode)
(keymap-set markdown-mode-map "M-<right>" 'markdown-demote)
(keymap-set markdown-mode-map "M-<left>" 'markdown-promote)
(keymap-set markdown-mode-map "M-<up>" 'markdown-move-up)
(keymap-set markdown-mode-map "M-<down>" 'markdown-move-down)
(keymap-set markdown-mode-map "C-<return>" 'markdown-insert-header-dwim)

;; Add a project command for terminal
;; Modified from https://github.com/emacs-mirror/emacs/blob/c77e35efd36f2c43e87066faa4257606d5c6f849/lisp/progmodes/project.el#L1279
(defun project-vterm ()
  "Start an inferior vterm in the current project's root directory.
If a buffer already exists for running a vterm in the project's root,
switch to it.  Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new inferior vterm buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (if (comint-check-proc vterm-buffer)
            (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
          (vterm vterm-buffer))
      (vterm (generate-new-buffer-name default-project-vterm-name)))))
(define-key project-prefix-map "t" 'project-vterm)
(add-to-list 'project-switch-commands '(project-vterm "Vterm") t)

;; Add a bind for creating non-project vterm buffer
(keymap-global-set "C-c v" (lambda (prefix)
                             "Open a terminal in the current directory.

With C-u as prefix argument, open a directory on the host."
                             (interactive "p")
                             (if (eq prefix 4)
                                 (let ((default-directory "~"))
                                     (vterm t))
                               (vterm t))))

;;; Custom functions
(defun current-project-root ()
  "Gets the root of the current project."
  (file-name-as-directory (expand-file-name (project-root (project-current)))))

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
(require 'envrc)
(envrc-global-mode)

;; Always use my desired shell (fish).
(setq config-shell "/run/current-system/sw/bin/fish")
(if (file-exists-p config-shell)
    (setq vterm-shell config-shell))

;; Run eglot-booster-mode on eglot start
;; (require 'eglot-booster)
;; (add-hook 'eglot-managed-mode-hook 'eglot-booster-mode)

;; Configure hunspell for markdown mode
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_AU")
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-buffer)

;; Hide unnecessary minor mode lighters
(require 'delight)
(delight '((editorconfig-mode nil editorconfig)
           (dtrt-indent-mode nil dtrt-indent)
           (auto-revert-mode nil autorevert)
           (eldoc-mode nil eldoc)))

;; Configure markdown-mode headings to look more like org-mode
(require 'font-lock)
(custom-set-faces
 '(markdown-header-face-1 ((t :inherit font-lock-function-name-face :weight bold)))
 '(markdown-header-face-2 ((t :inherit font-lock-variable-name-face :weight bold)))
 '(markdown-header-face-3 ((t :inherit font-lock-keyword-face :weight bold)))
 '(markdown-header-face-4 ((t :inherit font-lock-comment-face :weight bold)))
 '(markdown-header-face-5 ((t :inherit font-lock-type-face :weight bold)))
 '(markdown-header-face-6 ((t :inherit font-lock-constant-face :weight bold))))

;; Apply syntax highlighting to whole header
(setq markdown-fontify-whole-heading-line t)
;; Apply syntax highlighting to code blocks
(setq markdown-fontify-code-blocks-natively t)

;; Org configuration
(setq org-directory "~/Documents/iCloudOrg")
(setq org-default-notes-file (concat org-directory "/daily.org"))
(setq org-refile-targets `((,org-default-notes-file :maxlevel . 2)))

;; Set a custom-file so emacs doesn't freak out over this immutable init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
