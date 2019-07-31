;;; init.el --- Emacs Settings
;;; Commentary:
;;      Jae Yong Lee - Custon Emacs init.el file
;;; Code:
;; -*- lexical-binding: t -*-

;; sets debugging on
(setq debug-on-error t)

;; disable stupic spell check
(defconst *spell-check-support-enabled* nil)

;; check if the system is mac
(defconst *is-a-mac* (eq system-type 'darwin))

;; setup path for loading extensions
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- package level setups -- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-frame)
(require 'init-package)
(require 'init-projectile)
;;(require 'init-evil)
(require 'init-keyboard)
(require 'init-neotree)
(require 'init-multiple-cursor)

;; ivy depends on projectile
(require 'init-ivy)

;; search depends on ivy and mc
(require 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (python-mode counsel-projectile counsel swiper ivy neotree phi-search multiple-cursors ag use-package projectile helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
