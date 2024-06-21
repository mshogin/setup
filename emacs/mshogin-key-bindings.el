;; selections
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; zoom in/out
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;; helm
(global-set-key (kbd "M-m i i") 'helm-imenu)

;; folding
(global-set-key (kbd "M-m t f") 'evil-toggle-fold)

;; ;; dumb-jump
;; (global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-m .") 'xref-find-definitions-other-window)

;;
(global-set-key (kbd "M-<return> t r") 'projectile-run-project)

;; Open file at point
(global-set-key (kbd "M-m f a") 'find-file-at-point)

;; Google
(global-set-key (kbd "M-m a g g") 'google-this-line)

;; Google
(global-set-key (kbd "M-t") 'term-toggle-eshell)

(global-set-key (kbd "C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-next-like-this)
