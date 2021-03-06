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

;; dumb-jump
(global-set-key (kbd "M-.") 'xref-find-definitions)
