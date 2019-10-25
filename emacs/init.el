(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-o") 'origami-toggle-node)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-m j j ") 'avy-goto-char-2)
(global-set-key (kbd "M-m i i") 'helm-imenu)
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

(setq create-lockfiles nil)
(delete-selection-mode t)
(spacemacs/toggle-camel-case-motion-globally-on)
(smartparens-global-mode -1)

;(setq shell-file-name "zsh")
;(setq shell-command-switch "-ic")

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))
(global-set-key [?\C-\ ] 'toggle-maximize-buffer)


(defun my-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (interactive)
  (let ((fname (ffap-file-at-point)))
    (if fname
      (let ((line
             (save-excursion
               (goto-char (cadr ffap-string-at-point-region))
               (and (re-search-backward ":\\([0-9]+\\):"
                                        (line-beginning-position) t)
                    (string-to-number (match-string 1))))))
        ;; (message "file:%s,line:%s" fname line)
        (when (and (tramp-tramp-file-p default-directory)
                   (= ?/ (aref fname 0)))
          ;; if fname is an absolute path in remote machine, it will not return a tramp path,fix it here.
          (let ((pos (position ?: default-directory)))
            (if (not pos) (error "failed find first tramp indentifier ':'"))
            (setf pos (position ?: default-directory :start (1+ pos)))
            (if (not pos) (error "failed find second tramp indentifier ':'"))
            (setf fname (concat (substring default-directory 0 (1+ pos)) fname))))
        (message "fname:%s" fname)
        (find-file-existing fname)
        (when line (goto-line line)))
      (error "File does not exist."))))
(global-set-key (kbd "M-m f a") 'my-find-file-at-point-with-line)

;; GO MODE
;; (setq exec-path (append exec-path '("/usr/local/go/bin")))
;; (setq exec-path (append exec-path '("/home/mshogin/go/bin")))
;; (setenv "PATH"
;;         (concat
;;          "/usr/local/go/bin" ":"
;;          "/home/mshogin/go/bin" ":"
;;          (getenv "PATH")
;;          )
;;         )

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; (defun spacemacs/go-run-test-current-suite ()
;;   (interactive)
;;   (if (string-match "_test\\.go" buffer-file-name)
;;       (let ((test-method " -testify.m"
;;                            ))
;;         (save-excursion
;;           (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
;;           (spacemacs/go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))))
;;     (message "Must be in a _test.go file to run go-run-test-current-function")))
;; (global-set-key (kbd "M-m t s") 'spacemacs/go-run-test-current-suite)

;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (global-linum-mode t)
(global-set-key (kbd "M-.") 'spacemacs/jump-to-definition)
;; (require 'go-complete)
;; (add-hook 'completion-at-point-functions 'go-complete-at-point)

;; ORG MODE
(setq org-todo-keywords '((type "TODO" "INPROGRESS" "DONE" )))
;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

;; (use-package org-projectile
;;   :bind (("C-c n p" . org-projectile-project-todo-completing-read)
;;          ("C-c c" . org-capture))
;;   :config
;;   (progn
;;     (setq org-projectile-projects-file
;;           "/home/mshogin/projects.org")
;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;     (push (org-projectile-project-todo-entry) org-capture-templates))
;;   :ensure t)

(defun my-org-mode-hook ()
  (define-key global-map '[(control tab)] 'other-window)
  (define-key global-map '[(shift control tab)] 'my-other-window-back)
  (global-set-key '[(control tab)] 'other-window)
  (global-set-key '[(shift control tab)] 'my-other-window-back)
  (local-set-key '[(control tab)] 'other-window)
  (local-set-key '[(shift control tab)] 'my-other-window-back)
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; ORG mode

(add-to-list 'auto-mode-alist '("\\pipeline\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\pipeline-dev\\'" . groovy-mode))

(defun get-filename-and-line-number ()
  "Get filename and line namber: file.go:32"
  (interactive)
  (kill-new (
             concat (buffer-file-name) ":" (format-mode-line "%l")
                    ))
    ;; (kill-new ( buffer-file-name))

  )

(setq org-agenda-files (list "~/org/asap.org"
                             "~/org/family.org"
                             "~/org/job.org"))

(buffer-enable-undo)

(defun mshogin-split-and-helm-mini ()
  (interactive)
  (spacemacs/split-window-horizontally-and-switch)
  (helm-mini)
  )
(defun mshogin-split-h-and-helm-mini ()
  (interactive)
  (spacemacs/split-window-vertically-and-switch)
  (helm-mini)
  )



;(setq helm-show-completion-display-function #'helm-show-completion-default-display-function)
(global-set-key (kbd "M-r") 'helm-eshell-history)
;(global-set-key (kbd "M-SPC") 'goto-eshell)
(global-set-key (kbd "M-j") 'helm-eshell-history)
(global-set-key (kbd "M-m f o") 'find-file-other-window)
(global-set-key (kbd "M-m b j") 'dumb-jump-go)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-m w /") 'mshogin-split-and-helm-mini)
(global-set-key (kbd "M-m w -") 'mshogin-split-h-and-helm-mini)

(setq eshell-prompt-function
      (lambda ()
        (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
                (if (= (user-uid) 0) " # " " $ "))))
