;;; init.el --- Emacs Settings
;;
;;; Commentary:
;;      Jae Yong Lee - Custon Emacs init.el file
;;
;;
;;
;;; Code:
;;------------------------------------------------------------------------------
;;    Emacs Default Skin
;;------------------------------------------------------------------------------
(if window-system
    (progn
      ;; remove scroll bar
      (scroll-bar-mode -1)
          (tool-bar-mode -1)))
(menu-bar-mode -1)
(column-number-mode 1)

;; full screen
(if (eq system-type 'darwin)
    (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen) 
    (global-set-key (kbd "C-S-f") 'toggle-frame-fullscreen) 
)
;;------------------------------------------------------------------------------
;;    Package list setup
;;------------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;------------------------------------------------------------------------------
;;
;;    Package initializations
;;
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;exec-path
;;------------------------------------------------------------------------------
(when (eq system-type 'darwin) (exec-path-from-shell-initialize))
;;------------------------------------------------------------------------------
;;company mode
;;------------------------------------------------------------------------------
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (if (eq system-type 'darwin)
      (add-to-list 'company-backends 'company-rtags )))
;;------------------------------------------------------------------------------
;;helm mode
;;------------------------------------------------------------------------------
(use-package helm
  :init
  (helm-mode 1)
  :config
  (setq helm-split-window-in-side-p           t
        ;helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t )
  (global-set-key (kbd "M-x") 'helm-M-x))

;;------------------------------------------------------------------------------
;;flycheck
;;------------------------------------------------------------------------------
(use-package flycheck
  :init
  (global-flycheck-mode 1)
  :config
  (when (eq system-type 'gnu/linux)
    (add-hook 'c-mode-common-hook
              (lambda () (if (derived-mode-p 'c-mode 'c++-mode)
                             (flycheck-select-checker 'c/c++-gcc))))))


;;------------------------------------------------------------------------------
;;cpputil mode(for cmake)
;;------------------------------------------------------------------------------

(if (eq system-type 'darwin)
    (progn
      (use-package rtags
        :init
        (setq rtags-autostart-diagnostics t)
        (rtags-diagnostics)
        (setq rtags-completions-enabled t)
        :config
        (define-key c-mode-base-map (kbd "M-n") 'rtags-find-symbol-at-point)
        (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
        (define-key c-mode-base-map (kbd "M-t") 'rtags-find-file)
        (define-key c-mode-base-map (kbd "M-s") 'rtags-find-symbol)
        (define-key c-mode-base-map (kbd "M-r") 'rtags-find-references)
        (define-key c-mode-base-map (kbd "C-<") 'rtags-find-virtuals-at-point)
        (use-package cmake-ide
          :config
          (cmake-ide-setup)))

      (use-package cpputils-cmake
        :init
        (add-hook 'c-mode-common-hook
                  (lambda () (if (derived-mode-p 'c-mode 'c++-mode)
                                 (cppcm-reload-all))))
        :config
        (add-hook 'cppcm-reload-all-hook
                  (lambda () (setq
                              flycheck-gcc-include-path
                              flycheck-clang-include-path))))
      ))
;;------------------------------------------------------------------------------
;;emmet mode
;;------------------------------------------------------------------------------
(use-package emmet-mode
    :init
    (add-to-list 'load-path "~/emacs.d/emmet-mode")
    :config
    (add-hook 'sgml-mode-hook 'emmet-mode) 
    (add-hook 'css-mode-hook  'emmet-mode))

;;------------------------------------------------------------------------------
;;org-mode
;;------------------------------------------------------------------------------
(use-package org)

;;------------------------------------------------------------------------------
;;yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
    :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1))

;;------------------------------------------------------------------------------
;;neotree mode
;;------------------------------------------------------------------------------
(use-package neotree
    :config
    (global-set-key [f8] 'neotree-toggle))
    (add-hook 'neotree-mode-hook (lambda ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;------------------------------------------------------------------------------
;;evil-mode emulation
;;------------------------------------------------------------------------------

(use-package evil
    :init
    (use-package evil-leader
        :init
        (global-evil-leader-mode)
        :config
        (evil-leader/set-key
        "e" 'helm-find-files
        "b" 'switch-to-buffer
        "k" 'kill-buffer)    
        (evil-leader/set-leader "<SPC>")
        (use-package evil-nerd-commenter
            :config
            (evil-leader/set-key
            "ci" 'evilnc-comment-or-uncomment-lines
            "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
            "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
            "cc" 'evilnc-copy-and-comment-lines
            "cp" 'evilnc-comment-or-uncomment-paragraphs
            "cr" 'comment-or-uncomment-region
            "cv" 'evilnc-toggle-invert-comment-line-by-line   
            "\\" 'evilnc-comment-operator)))
    :config
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-w q") 'delete-window)
    (use-package evil-surround
        :config
        (global-evil-surround-mode 1)))

;;------------------------------------------------------------------------------
;; key chord mode
;;------------------------------------------------------------------------------
(use-package key-chord
  :init
  (key-chord-mode 1)
  :config
  ;; emacs, insert, motion, normal, visual, replace, operator
  (key-chord-define evil-normal-state-map "cp" 'c++-mode)
  (key-chord-define evil-normal-state-map ";;" 'move-end-of-line)
  (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-operator-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state))

;;------------------------------------------------------------------------------
;; Write room mode
;;------------------------------------------------------------------------------
(use-package writeroom-mode
  :config
  (global-unset-key (kbd "C-x C-w"))
  (global-set-key (kbd "C-x C-w") 'writeroom-mode))
;;------------------------------------------------------------------------------
;;
;;    Syntax / Key bindings
;;
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;asm-mode syntax
;;------------------------------------------------------------------------------
(add-hook 'asm-mode-hook
    (lambda () (interactive) (modify-syntax-entry ?# "< b")))
    
;;------------------------------------------------------------------------------
;;c/c++-mode syntax
;;------------------------------------------------------------------------------
(defun my-cc-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)
;;------------------------------------------------------------------------------
;;key bindings
;;------------------------------------------------------------------------------
(global-set-key (kbd "<f9>")
                (lambda () (interactive)
                  (find-file-other-window user-init-file)))
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b")(lambda () (interactive) (ibuffer t)))

;;------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (writeroom-mode yasnippet emmet-mode)))
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray17" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Inconsolata")))))
