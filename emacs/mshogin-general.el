(setq create-lockfiles nil)

(delete-selection-mode 1)

(spacemacs/toggle-camel-case-motion-globally-on)

;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
;;                 (if (= (user-uid) 0) " # " " $ "))))

;; (add-to-list 'auto-mode-alist '("\\pipeline\\'" . groovy-mode))

(defun mark-from-point-to-end-of-line ()
  "Marks everything from point to end of line"
  (interactive)
  (set-mark (line-end-position))
  (activate-mark))

;; folding
(global-set-key (kbd "M-e") 'mark-from-point-to-end-of-line)
(global-set-key (kbd "C-e") 'end-of-line)

(setq magit-list-refs-sortby "-creatordate")

(setenv "PATH" "/home/mshogin/.pyenv/shims:/home/mshogin/.pyenv/bin:/.gem/bin:/home/mshogin/.poetry/bin:/home/mshogin/.poetry/bin:/home/mshogin/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/opt/puppetlabs/bin:/home/mshogin/.cargo/bin/")

(set-face-attribute 'region nil :background "#821282")

(reverse-im-mode)
(reverse-im-activate "russian-computer")
