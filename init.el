;; PACKAGES 
;; setup packages
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") 
    '("gnu-devel" . "https://elpa.gnu.org/devel/"))

(setq package-install-upgrade-built-in t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; osx copy and paste
;(defun wgr/paste-from-osx ()
;  (shell-command-to-string "pbpaste"))
;(defun wgr/copy-to-osx (text &optional push)
;  (let ((process-connection-type nil))
;    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;      (process-send-string proc text)
;      (process-send-eof proc))))
;(setq interprogram-cut-function 'wgr/copy-to-osx)
;(setq interprogram-paste-function 'wgr/paste-from-osx)

;; BASIC STUFF
;; disable menu stuff
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; stop from overwritting config file
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)

;; save session
 (desktop-save-mode 1)

;; command autocomplete
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

;; Keybindings preview
(use-package which-key
    :config
    (which-key-mode))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; file tree
(use-package neotree
  :ensure t)

;; line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; font
(set-face-attribute 'default nil :height 105)

;; Theme
(use-package solarized-theme)
(load-theme 'solarized-light t)

;; git
(use-package magit
  :ensure t)
;; show changes in file
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; VIM AND EVIL MODE
;; Vim style undo not needed for emacs 28
(use-package undo-fu)
;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; LANGUAGES AND LSP
(use-package lsp-mode
  :ensure
  :commands lsp
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  ("M-j" . lsp-ui-imenu)
  ("M-?" . lsp-find-references)
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-signature-auto-activate t)
  (lsp-idle-delay 0.3)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-enable-which-key-integration t))

 (use-package lsp-ui
   :ensure
   :commands lsp-ui-mode
   :custom
   (lsp-ui-peek-always-show nil)
   (lsp-ui-sideline-show-hover nil)
   (lsp-ui-doc-enable t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; completion
(use-package company
  :ensure
  :custom
  (company-idle-delay nil) ;; how long to wait until popup
  :bind
  (:map company-mode-map
	("C-SPC". company-complete-common))
  :config
  ;; Trigger autoomplete binding
  (global-company-mode))

;; Rust
(use-package rustic
  :ensure
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer "clippy"
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t))
  ;;(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; C
(add-hook 'c-mode-hook 'lsp)
