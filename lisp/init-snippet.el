;;; init-helm.el --- javascript initialization
;;; Commentary:
;;; Code:
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
  (yas-global-mode 1)
(provide 'init-snippet)
;;; init-helm ends here
