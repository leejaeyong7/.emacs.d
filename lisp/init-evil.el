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
      "p" 'counsel-projectile-find-file
      "f" 'counsel-projectile-find-file
      "." 'counsel-find-file
      "/" 'counsel-projectile-ag
      "b" 'ivy-switch-buffer
      "s" 'swiper
      "k" 'kill-buffer
      "g" 'magit-status
      "=" 'writeroom-mode
      "\\" 'toggle-frame-fullscreen
      "cl" 'evilnc-comment-or-uncomment-lines
      "ui" (lambda () (interactive)
             (find-file-other-window user-init-file))))
  (use-package evil-mc
    :init
    (evil-mc-mode 1))
  :config
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-w q") 'delete-window))

(provide 'init-evil)
;;; init-evil ends here
