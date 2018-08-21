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
      "d" 'counsel-projectile-find-dir
      "f" 'counsel-projectile-find-file
      "p" 'counsel-projectile-switch-project
      "." 'counsel-find-file
      "/" 'counsel-projectile-ag
      "b" 'ivy-switch-buffer
      "s" 'swiper
      "k" 'kill-buffer
      "g" 'magit-status
      "e" 'neotree-find
      "t" 'neotree-toggle
      "=" 'writeroom-mode
      "\\" 'toggle-frame-fullscreen
      "cl" 'evilnc-comment-or-uncomment-lines
      "a" 'org-agenda
      "z" 'org-show-todo-tree
      "c" 'org-archive-subtree
      "ui" (lambda () (interactive)
             (find-file-other-window user-init-file))
      "op" (lambda () (interactive)
             (find-file-other-window *global-todo-org*))))
  :config
  ; global maps
  (evil-define-key 'normal 'global (kbd "/") 'swiper)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal 'global (kbd "C-w q") 'delete-window)
  (evil-define-key 'normal 'global (kbd "zs") 'outline-toggle-children)
  (evil-define-key 'normal 'global (kbd "zd") 'outline-show-children)
  (evil-define-key 'normal 'global (kbd "zf") 'outline-show-subtree)
  (evil-define-key 'normal 'global (kbd "zg") 'outline-hide-subtree)
  (evil-define-key 'insert 'global (kbd "TAB") 'yas-expand)
  ; neotree maps
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "mc") 'neotree-create-node)
  (evil-define-key 'normal neotree-mode-map (kbd "md") 'neotree-delete-node)
  (evil-define-key 'normal neotree-mode-map (kbd "me") 'neotree-rename-node)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "u") 'neotree-select-up-node)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  ; orgmode maps
  (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "T") 'org-todo-list)
  (evil-define-key 'normal org-mode-map (kbd "H") 'org-shiftleft)
  (evil-define-key 'normal org-mode-map (kbd "L") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "J") 'org-insert-heading-after-current)
  (evil-define-key 'normal org-mode-map (kbd "K") 'org-insert-subheading)
  (evil-define-key 'normal org-mode-map (kbd "<") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd ">") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "mk") 'org-move-subtree-up)
  (evil-define-key 'normal org-mode-map (kbd "mj") 'org-move-subtree-down)
  )
(provide 'init-evil)
