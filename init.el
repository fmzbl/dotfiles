;; PACKAGES 
;; setup packages
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") 
    '("gnu-devel" . "https://elpa.gnu.org/devel/"))

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
(defun wgr/paste-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun wgr/copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'wgr/copy-to-osx)
(setq interprogram-paste-function 'wgr/paste-from-osx)

;; BASIC STUFF
;; disable menu stuff
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; stop from overwritting config file
(setq disabled-command-function nil)
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

;; line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; font
(set-face-attribute 'default nil :height 140)

;; theme
(use-package solarized-theme
  :init
  (load-theme 'solarized-light t))

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
  :config
  (evil-mode 1))


;; LANGUAGES AND LSP
;(use-package lsp-mode
;  :commands lsp
;  :init
;  ;(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;  :config
;  (require 'lsp-clients))

;(use-package lsp-ui)

(use-package which-key
    :config
    (which-key-mode))

(use-package rust-mode)
