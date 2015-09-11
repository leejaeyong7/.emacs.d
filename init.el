;;--------------------------------------------------------------------------------------------------
;;
;;    Basic customizations
;;
;;--------------------------------------------------------------------------------------------------

(set-face-attribute 'default nil
 :inherit nil
 :stipple nil
 :background "gray17"
 :foreground "white"
 :inverse-video nil
 :box nil
 :strike-through nil
 :overline nil
 :underline nil
 :slant 'normal
 :weight 'normal
 :height 120 
 :width 'normal
 :foundry "outline"
 :family "Consolas")



(if window-system
    (tool-bar-mode -1))

;;--------------------------------------------------------------------------------------------------
;;
;;    Package list setup
;;
;;--------------------------------------------------------------------------------------------------

;lisp loadpath
;(when (eq system-type 'gnu/linux) (add-to-list 'load-path "~/.emacs.d/lisp"))
(require 'package) ;; You might already have this line
(setq package-list '(multiple-cursors emmet-mode org ssh yasnippet flycheck evil writeroom-mode))
(setq debug-on-error t)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
			               ("melpa" . "http://melpa.org/packages/")
			 ))
(package-initialize) ;; You might already have this line
;(package-refresh-contents)

;windows ssh activation
(when (and (eq system-type 'windows-nt) (file-exists-p "C:\Program Files (x86)/PuTTY/plink.exe"))
  (add-to-list 'package-list 'tramp))

;check system for package-list addition
;lisp interaction

(when (or (when (eq system-type 'darwin)(file-exists-p "/usr/local/bin/sbcl"))(when (eq system-type 'gnu/linux) (file-exists-p "/bin/sbcl"))) (add-to-list 'package-list 'slime))

;haskell interaction
(when (or (when (eq system-type 'gnu/linux) (file-exists-p "/bin/ghc"))
          (when (eq system-type 'windows-nt) (file-directory-p "~/ghc"))
          (when (eq system-type 'darwin) (or (file-exists-p "/opt/local/bin/ghc") (file-exists-p "/usr/bin/ghc"))))
  (progn
    (setq package-list (append package-list '(haskell-mode ghc cl-lib company company-ghc flycheck-haskell)))
    (when (eq system-type 'darwin) (add-to-list 'package-list 'exec-path-from-shell))))




;;--------------------------------------------------------------------------------------------------
;;
;;    Installation process
;;
;;--------------------------------------------------------------------------------------------------
;fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;;--------------------------------------------------------------------------------------------------
;;
;;    Package initializations
;;
;;--------------------------------------------------------------------------------------------------

(require 'multiple-cursors)
(global-set-key (kbd "<f9>") (lambda () (interactive) (find-file-other-window user-init-file)))
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-n") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-S-m") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-dwim)
(setq mc/cmds-to-run-for-all (append mc/cmds-to-run-for-all
                                     '(c-electric-paren
                                       evil-append
                                       evil-append-line
                                       evil-backward-char
                                       evil-backward-paragraph
                                       evil-backward-word-begin
                                       evil-change
                                       evil-delete
                                       evil-delete-backward-char-and-join
                                       evil-delete-char
                                       evil-downcase
                                       evil-end-of-line
                                       evil-escape-insert-state
                                       evil-first-non-blank
                                       evil-force-normal-state
                                       evil-forward-char
                                       evil-forward-paragraph
                                       evil-forward-word-begin
                                       evil-forward-word-end
                                       evil-goto-line
                                       evil-insert
                                       evil-jump-item
                                       evil-next-line
                                       evil-normal-state
                                       evil-paste-after
                                       evil-previous-line
                                       evil-search-next
                                       evil-snipe-f
                                       evil-visual-char
                                       evil-visual-line
                                       evil-yank)))
;;--------------------------------------------------------------------------------------------------


(require 'emmet-mode)
(add-to-list 'load-path "~/emacs.d/emmet-mode")
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;--------------------------------------------------------------------------------------------------
;; (when (package-installed-p 'slime)
;;   (require 'slime)
;;   ;; Set your lisp system and, optionally, some contribs
;;   (when (eq system-type 'darwin)(setq inferior-lisp-program "/usr/local/bin/sbcl"))
;;   (when (eq system-type 'gnu/linux) (setq inferior-lisp-program "/bin/sbcl"))
;;  (setq slime-contribs '(slime-fancy)))

;;--------------------------------------------------------------------------------------------------
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;;--------------------------------------------------------------------------------------------------
(when (package-installed-p 'haskell-mode)
  (if (eq system-type 'gnu/linux)(add-to-list 'exec-path "~/.cabal/bin"))
  (if (eq system-type 'windows-nt) (add-to-list 'exec-path "~/cabal/bin"))
  (if (eq system-type 'darwin)
      (progn
        (add-to-list 'exec-path "~/Library/Haskell/bin")
        (add-to-list 'exec-path "/opt/local/bin")
        (exec-path-from-shell-initialize)))
  ;(if (eq system-type 'windows-nt) (setq ghc-interactive-command "ghc-modi"))
  (require 'haskell-mode)
  ;(load "haskell-mode-autoloads")
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (require 'company)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'company-mode)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  (setq ghc-debug t)
  ;haskell mode keymaps
  (custom-set-variables
   '(haskell-process-type 'ghci)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;company-mode

  (add-to-list 'company-backends 'company-ghc)
  )
;;--------------------------------------------------------------------------------------------------
(require 'ssh)
(add-hook 'ssh-mode-hook
          (lambda ()
            (setq ssh-directory-tracking-mode t)
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))
;;--------------------------------------------------------------------------------------------------
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;;--------------------------------------------------------------------------------------------------
(when (and (eq system-type 'windows-nt) (package-installed-p 'tramp))
  (progn
    (add-hook 'find-file-hook (lambda ()
	  (when (symbol-value 'buffer-file-name)
	    (when
            (string= (substring (symbol-value 'buffer-file-name) 1 6) "plink")
        (setq buffer-auto-save-file-name nil)))))
    (setq tramp-default-method "plink")
 
    (setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files (x86)\\PuTTY"))
    (setq exec-path (append exec-path '("C:\Program Files (x86)/PuTTY")))
    ))
;;--------------------------------------------------------------------------------------------------

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

;;--------------------------------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

 

;;--------------------------------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
(global-undo-tree-mode 1)



;;--------------------------------------------------------------------------------------------------
(add-hook 'asm-mode-hook
          (lambda () (interactive) (modify-syntax-entry ?# "< b")))
    
;; backup data-----------


;;--------------------------------------------------------------------------------------------------
;;
;;    Global ket setups
;;
;;--------------------------------------------------------------------------------------------------

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b")(lambda () (interactive) (ibuffer t)))
(global-unset-key [(shift f11)])

(if (eq system-type 'darwin) (global-set-key (kbd "C-c C-f") 'writeroom-mode) (global-set-key [(shift f11)] 'writeroom-mode))


;;--------------------------------------------------------------------------------------------------


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(package-selected-packages
        (quote
         (writeroom-mode flycheck-haskell flycheck yasnippet ssh multiple-cursors emmet-mode company-ghc)))
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
