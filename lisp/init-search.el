(defun phi-search-or-swiper()
  "Search with Swiper if multiple-cursor mode is not enabled.\n 
   Otherwise use phi-search"
  (interactive)
  (if (boundp 'multiple-cursors-mode)
      (call-interactively 'swiper)
    (call-interactively 'phi-search)))
(defun phi-search-backward-or-swiper()
  "Search with Swiper if multiple-cursor mode is not enabled.\n
   Otherwise use phi-search-backward"
  (interactive)
  (if (boundp 'multiple-cursors-mode)
      (call-interactively 'swiper-backward)
    (call-interactively 'phi-search-backward)))


(use-package phi-search
  :init
  (use-package swiper
    :init
    (global-set-key (kbd "C-s") 'phi-search-or-swiper)
    (global-set-key (kbd "C-r") 'phi-search-backward-or-swiper)    
    )
  )
(provide 'init-search)
