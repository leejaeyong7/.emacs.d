(use-package ivy
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  ;; use swiper for search
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; add additional projectile config iff using ivy
  (use-package counsel-projectile
    :config
    (counsel-projectil-mode 1))
  )

(provide 'init-ivy)
