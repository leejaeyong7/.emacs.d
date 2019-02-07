;;; init.el --- Emacs Settings
;;; Commentary:
;;      Jae Yong Lee - Custon Emacs init.el file
;;; Code:
;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; global variable and path setup
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *global-todo-org* "~/Dropbox/Notes/Todo.org")

;;------------------------------------------------------------------------------
;; Bootstrap config
;;------------------------------------------------------------------------------

;; Emacs Default modes
(if window-system
    (progn
      ;; remove scroll bar
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(menu-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(recentf-mode 1)

;;------------------------------------------------------------------------------
;;    Package list setup
;;------------------------------------------------------------------------------

;; local requires
(require 'init-package)
(require 'init-javascript)
(require 'init-cpp)
(require 'init-projectile)
(require 'init-git)
(require 'init-snippet)
;(require 'init-flycheck)
(require 'init-org)
(require 'init-auto-complete)
(require 'init-minibuffer)
(require 'init-evil)
(require 'init-writeroom)
(require 'init-neotree)
(require 'init-undo-tree)

;; platform specific
(when *is-a-mac*
  (require 'init-mac-path-setup))

;;------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(fringe-mode nil nil (fringe))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js-js-switch-tabs t)
 '(js-switch-indent-offset 2)
 '(package-selected-packages
   (quote
    (vue-mode dockerfile-mode cmake-mode esup org counsel ivy writeroom-mode yasnippet emmet-mode)))
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(cursor ((t (:background "White"))))
 '(escape-glyph ((t (:foreground "green1"))))
 '(font-lock-comment-face ((t (:foreground "orange1"))))
 '(fringe ((t (:background "gray17"))))
 '(minibuffer-prompt ((t (:foreground "green1"))))
 '(mode-line ((t (:background "dark green" :foreground "wheat1" :box nil))))
 '(region ((t (:background "DarkOrchid4")))))
