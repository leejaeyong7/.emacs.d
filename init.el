;;; init.el --- Emacs Settings
;;
;;; Commentary:
;;      Jae Yong Lee - Custon Emacs init.el file
;;
;;; Code:

;;------------------------------------------------------------------------------
;;    Emacs Default modes
;;------------------------------------------------------------------------------
(if window-system
    (progn
      ;; remove scroll bar
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(menu-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)

;; full screen
(if (eq system-type 'darwin)
    (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen) 
    (global-set-key (kbd "C-S-f") 'toggle-frame-fullscreen))

;;------------------------------------------------------------------------------
;;    Package list setup
;;------------------------------------------------------------------------------
; list the packages you want
(setq package-list '(
                     package
                     magit
                     helm
                     projectile
                     helm-projectile
                     js2-mode
                     multiple-cursors
                     flycheck
                     emmet-mode
                     org
                     yasnippet
                     expand-region
                     auto-complete
                     slime
                     neotree
                     ))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; (require 'package)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (when (< emacs-major-version 24)
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; (package-initialize)

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
;; magit settings
;;------------------------------------------------------------------------------
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))
  ;:defer t)

;;------------------------------------------------------------------------------
;;helm mode
;;------------------------------------------------------------------------------
(use-package helm
  :init
  (helm-mode 1)
  (use-package helm-ac)
  (use-package projectile
    :init
    (use-package helm-projectile
      :config
      (helm-projectile-on)              
      ))
  :config
  (setq helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
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
;; evil mode
;;------------------------------------------------------------------------------
;; (use-package evil
;;     :init
;;     (use-package evil-mc
;;       :init
;;       (evil-mc-mode 1)
;;       :config
;;       (global-set-key (kbd "C-c s-d") 'evil-mc-make-all-cursors)
;;       ;; (global-set-key (kbd "C-s-d") (evil-mc-undo-all-cursors))
;;       (global-set-key (kbd "s-d") 'evil-mc-make-and-goto-next-match)
;;       (global-set-key (kbd "C-s-d") 'evil-mc-skip-and-goto-next-match))
;;     (use-package evil-leader
;;         :init
;;         (global-evil-leader-mode)
;;         :config
;;         (evil-leader/set-key
;;         "e" 'helm-find-files
;;         "b" 'switch-to-buffer
;;         "k" 'kill-buffer)    
;;         (evil-leader/set-leader "<SPC>")
;;         (use-package evil-nerd-commenter
;;           :init
;;           :config
;;             (evil-leader/set-key
;;             "ci" 'evilnc-comment-or-uncomment-lines
;;             "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;             "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
;;             "cc" 'evilnc-copy-and-comment-lines
;;             "cp" 'evilnc-comment-or-uncomment-paragraphs
;;             "cr" 'comment-or-uncomment-region
;;             "cv" 'evilnc-toggle-invert-comment-line-by-line   
;;             "\\" 'evilnc-comment-operator)))
;;     :config
;;     (evil-mode 1)
;;     (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;     (define-key evil-normal-state-map (kbd "C-w q") 'delete-window)
;;     (use-package evil-surround
;;         :config
;;         (global-evil-surround-mode 1)))

;;------------------------------------------------------------------------------
;; key chord mode
;;------------------------------------------------------------------------------
;; (use-package key-chord
;;   :init
;;   (key-chord-mode 1)
;;   :config
;;   ;; emacs, insert, motion, normal, visual, replace, operator
;;   (key-chord-define evil-normal-state-map "cp" 'c++-mode)
;;   (key-chord-define evil-normal-state-map ";;" 'move-end-of-line)
;;   (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-operator-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-replace-state-map "jk" 'evil-normal-state))
;;------------------------------------------------------------------------------
;; auto complete mode
;;------------------------------------------------------------------------------
(use-package expand-region
  :init
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))
;;------------------------------------------------------------------------------
;; auto complete mode
;;------------------------------------------------------------------------------
(use-package auto-complete
  :init
  (auto-complete-mode 1))
;;------------------------------------------------------------------------------
;; auto complete mode
;;------------------------------------------------------------------------------
(use-package slime
  :init
  (slime-setup '(slime-fancy))
  (setq inferior-lisp-program "sbcl")
  (use-package slime-autoloads)
  :defer t)
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
(add-hook 'c-mode-common-hook (lambda () (setq comment-start "/* "
     comment-end " */")))
;;------------------------------------------------------------------------------
;; js mode syntax
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load "js2-mode"
  '(progn
     (setq-default js2-basic-offset 2)))
;;------------------------------------------------------------------------------
;;key bindings
;;------------------------------------------------------------------------------
(global-set-key (kbd "<f9>")
                (lambda () (interactive)
                  (find-file-other-window user-init-file)))
(global-set-key (kbd "C-c C-p") 'projectile-mode)
(global-set-key (kbd "C-x C-g") 'neotree-toggle)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

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
 '(package-selected-packages (quote (writeroom-mode yasnippet emmet-mode)))
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray17" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Inconsolata"))))
 '(cursor ((t (:background "White"))))
 '(escape-glyph ((t (:foreground "green1"))))
 '(font-lock-comment-face ((t (:foreground "orange1"))))
 '(fringe ((t (:background "gray17"))))
 '(minibuffer-prompt ((t (:foreground "green1"))))
 '(mode-line ((t (:background "dark green" :foreground "wheat1" :box nil))))
 '(region ((t (:background "DarkOrchid4")))))
