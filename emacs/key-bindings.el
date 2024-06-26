;; selections
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; zoom in/out
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;; helm
(global-set-key (kbd "M-m i i") 'helm-imenu)
(global-set-key (kbd "M-m e e") 'ein:worksheet-execute-cell)
(global-set-key (kbd "M-m e i") 'ein:worksheet-execute-cell-and-goto-next-km)

;; folding
(global-set-key (kbd "M-m t f") 'evil-toggle-fold)

;; dumb-jump
(global-set-key (kbd "M-.") 'xref-find-definitions)

(global-set-key (kbd "C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-next-like-this)

;; copilot
(global-set-key (kbd "M-m a w c") 'copilot-complete)
(global-set-key (kbd "M-m a w a") 'copilot-accept-completion)
(global-set-key (kbd "M-m a w n") 'copilot-next-completion)
