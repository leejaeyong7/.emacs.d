(use-package projectile
  :init
  (projectile-mode 1)
  (global-set-key (kbd "s-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (use-package ag))
(provide 'init-projectile)
