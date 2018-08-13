;;; init-evil.el --- javascript initialization
;;; Commentary:
;;; Code:
(use-package evil
  :init
  (evil-mode 1)
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-nerd-commenter)
  (use-package evil-leader
    :init
    (global-evil-leader-mode)
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'counsel-M-x
      "r" 'counsel-recentf
      "d" 'counsel-projectile-find-dir
      "f" 'counsel-projectile-find-file
      "p" 'counsel-projectile-switch-project
      "." 'counsel-find-file
      "/" 'counsel-projectile-ag
      "b" 'ivy-switch-buffer
      "s" 'swiper
      "k" 'kill-buffer
      "g" 'magit-status
      "e" 'neotree-find
      "t" 'neotree-toggle
      "=" 'writeroom-mode
      "\\" 'toggle-frame-fullscreen
      "cl" 'evilnc-comment-or-uncomment-lines
      "ui" (lambda () (interactive)
             (find-file-other-window user-init-file))))
  :config
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-w q") 'delete-window)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))
(provide 'init-evil)
;;; init-evil ends here
