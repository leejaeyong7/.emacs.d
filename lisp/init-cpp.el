;;; init-javascript.el --- javascript initialization
;;; Commentary:
;;; Code:
(defun my-cc-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset 2))
(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)
(add-hook 'c-mode-common-hook (lambda () (setq comment-start "/* "
     comment-end " */")))
(provide 'init-cpp)
;;; init-javascript ends here
