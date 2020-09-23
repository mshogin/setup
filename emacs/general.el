(setq create-lockfiles nil)

(delete-selection-mode t)

(spacemacs/toggle-camel-case-motion-globally-on)

(setq eshell-prompt-function
      (lambda ()
        (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
                (if (= (user-uid) 0) " # " " $ "))))

(add-to-list 'auto-mode-alist '("\\pipeline\\'" . groovy-mode))

(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))
