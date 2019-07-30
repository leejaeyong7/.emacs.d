;;; -- lisp functions -- ;;;
(defun edit-init-file ()
  "Edits user init file"
  (interactive)
  (find-file-other-window user-init-file))

;;; -- key board shortcuts -- ;;;
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-c i") 'edit-init-file)
(windmove-default-keybindings)

(provide 'init-keyboard)
