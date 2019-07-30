(use-package multiple-cursors
  :init
  (multiple-cursors-mode 1)
  (global-set-key (kbd "C-c l") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-S-c C-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
  )
(provide 'init-multiple-cursor)
