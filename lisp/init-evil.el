;;; init-evil.el --- javascript initialization
;;; Commentary:
;;; Code:
(use-package evil
  :init
  (evil-mode 1)
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-nerd-commenter)
  (use-package evil-leader
    :init
    (global-evil-leader-mode)
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'counsel-M-x
      "r" 'counsel-recentf
      "p" 'counsel-projectile-find-dir
      "f" 'counsel-projectile-find-file
      "." 'counsel-find-file
      "/" 'counsel-projectile-ag
      "b" 'ivy-switch-buffer
      "s" 'swiper
      "k" 'kill-buffer
      "g" 'magit-status
      "=" 'writeroom-mode
      "\\" 'toggle-frame-fullscreen
      "cl" 'evilnc-comment-or-uncomment-lines
      "ui" (lambda () (interactive)
             (find-file-other-window user-init-file))))
  (use-package evil-mc
    :init
    (defun col-at-point (point)
      (save-excursion (goto-char point) (current-column)))

    (defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
      (end-of-line)
      (when (> endcol (current-column))
        (insert-char ?\s (- endcol (current-column))))
      (move-to-column (- endcol 1))
      (unless (= (line-number-at-pos) orig-line)
        (evil-mc-make-cursor-here)))

    (defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
      (end-of-line)
      (move-to-column startcol)
      (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
        (evil-mc-make-cursor-here)))

    (defun evil--mc-make-vertical-cursors (beg end func)
      (evil-mc-pause-cursors)
      (apply-on-rectangle func
                          beg end (line-number-at-pos (point)))
      (evil-mc-resume-cursors)
      (evil-normal-state))

    (defun evil-mc-insert-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
      (move-to-column (min (col-at-point beg) (col-at-point end))))

    (defun evil-mc-append-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
      (move-to-column (- (max (col-at-point beg) (col-at-point end)) 1)))

    :config
    (define-key 'evil-visual-state-map "gI" 'evil-mc-insert-vertical-cursors)
    (define-key 'evil-visual-state-map "gA" 'evil-mc-append-vertical-cursors)
    )
  :config
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-w q") 'delete-window))

(provide 'init-evil)
;;; init-evil ends here
