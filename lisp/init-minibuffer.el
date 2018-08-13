(use-package ivy
  :init
  (ivy-mode 1)
  (use-package counsel
    :init
    (use-package counsel-projectile)
    :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    ))
(provide 'init-minibuffer)
