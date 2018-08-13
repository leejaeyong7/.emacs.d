;;; init-javascript.el --- javascript initialization
;;; Commentary:
;;; Code:
(use-package js2-mode
  :init
  (setq-default js2-basic-offset 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(provide 'init-javascript)
;;; init-javascript ends here
