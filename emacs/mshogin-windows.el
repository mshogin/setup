(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(global-set-key [?\C-\ ] 'toggle-maximize-buffer)

(defun mshogin-split-window-horizontally-and-helm-mini ()
  (interactive)
  (spacemacs/split-window-horizontally-and-switch)
  (helm-mini)
  )
(global-set-key (kbd "M-m w /") 'mshogin-split-window-horizontally-and-helm-mini)

(defun mshogin-split-window-vertically-and-helm-mini ()
  (interactive)
  (spacemacs/split-window-vertically-and-switch)
  (helm-mini)
  )
(global-set-key (kbd "M-m w -") 'mshogin-split-window-vertically-and-helm-mini)

(defun toggle-term-buffer () "Toggle terminal buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(global-set-key [?\C-\ ] 'toggle-maximize-buffer)
