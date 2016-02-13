;;------------------------------------------------------------------------------
;;
;;    Basic customizations
;;
;;------------------------------------------------------------------------------

;; if window system remove toolbar
(if window-system
    (progn
    ;; remove scroll bar
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))
;; remove menu bar
(menu-bar-mode -1)
(column-number-mode 1)
(if (eq system-type 'darwin)
    (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen) 
)




;;------------------------------------------------------------------------------
;;
;;    Package list setup
;;
;;------------------------------------------------------------------------------

;lisp loadpath
(require 'package)
(setq package-list '(emmet-mode
                     org
                     ssh
                     yasnippet
                     flycheck
                     neotree
                     projectile
                     flx-ido
                     writeroom-mode
                     exec-path-from-shell
                     evil
                     evil-nerd-commenter
                     evil-leader
                     evil-surround
                     evil-numbers
                     auto-complete
                     ))

(setq debug-on-error t)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ))
(package-initialize)

;add homebrew path on os x
(when (eq system-type 'darwin) (exec-path-from-shell-initialize))

;;add custom path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc"))
;;------------------------------------------------------------------------------
;;
;;    Installation process
;;
;;------------------------------------------------------------------------------
;fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;------------------------------------------------------------------------------
;;
;;    Package initializations
;;
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;;emmet mode
;;------------------------------------------------------------------------------
(require 'emmet-mode)
(add-to-list 'load-path "~/emacs.d/emmet-mode")
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;------------------------------------------------------------------------------
;;org-mode
;;------------------------------------------------------------------------------
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;;------------------------------------------------------------------------------
;;yasnippet
;;------------------------------------------------------------------------------
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;;------------------------------------------------------------------------------
;;flycheck
;;------------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;------------------------------------------------------------------------------
;;asm-mode syntax
;;------------------------------------------------------------------------------
;; (add-hook 'asm-mode-hook
;;           (lambda () (interactive) (modify-syntax-entry ?# "< b")))
    
;;------------------------------------------------------------------------------
;;c-mode syntax
;;------------------------------------------------------------------------------
(setq c-default-style "linux"
      c-basic-offset 4)

;;------------------------------------------------------------------------------
;;c++-mode syntax
;;------------------------------------------------------------------------------
(defun my-cc-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)
;;------------------------------------------------------------------------------
;;auto-complete mode
;;------------------------------------------------------------------------------
(global-auto-complete-mode 1)

;;------------------------------------------------------------------------------
;;neotree mode
;;------------------------------------------------------------------------------
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(add-hook 'neotree-mode-hook
    (lambda ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(setq projectile-switch-project-action 'neotree-projectile-action)
    
;;------------------------------------------------------------------------------
;;projectile mode
;;------------------------------------------------------------------------------
(projectile-global-mode)

;;------------------------------------------------------------------------------
;;ido mode
;;------------------------------------------------------------------------------
(require 'ido)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
    

;;------------------------------------------------------------------------------
;;evil-mode emulation
;;------------------------------------------------------------------------------

(require 'evil)
(require 'evil-leader)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(global-evil-leader-mode)
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)    
(evil-leader/set-leader ",")
(global-evil-leader-mode)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)
(define-key evil-normal-state-map (kbd "C-w q") 'delete-window)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)


;;------------------------------------------------------------------------------
;;
;;    Global ket setups
;;
;;------------------------------------------------------------------------------
(global-set-key (kbd "<f9>")
                (lambda () (interactive)
                  (find-file-other-window user-init-file)))
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b")(lambda () (interactive) (ibuffer t)))
(global-unset-key (kbd "C-x C-w"))
(global-set-key (kbd "C-x C-w") 'writeroom-mode) 

;;------------------------------------------------------------------------------


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (writeroom-mode flycheck yasnippet ssh emmet-mode)))
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray17" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Inconsolata")))))
