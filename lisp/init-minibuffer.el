(use-package ivy
  :init
  (ivy-mode 1)
  (use-package counsel
    :init
    (use-package counsel-projectile
      :init
      (setq projectile-keymap-prefix (kbd "C-c C-p")))
    :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-x C-m") 'ivy-immediate-done)
    ))
(provide 'init-minibuffer)
