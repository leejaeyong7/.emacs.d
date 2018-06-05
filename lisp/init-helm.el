;;; init-helm.el --- javascript initialization
;;; Commentary:
;;; Code:
(require 'helm)
(require 'helm-ag)
(require 'helm-projectile)
(helm-mode 1)
(helm-projectile-on)

(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(setq helm-split-window-in-side-p        t
      helm-move-to-line-cycle-in-source  t
      helm-ff-search-library-in-sexp     t
      helm-recentf-fuzzy-match           t
      helm-buffers-fuzzy-match           t
      helm-M-x-fuzzy-match               t
)
(provide 'init-helm)
;;; init-helm ends here
