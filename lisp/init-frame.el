;; Emacs Default modes
(if window-system
    (progn
      ;; remove scroll bar
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(menu-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(provide 'init-frame)
