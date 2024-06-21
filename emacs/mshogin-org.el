(global-set-key (kbd "C-t") 'org-todo)
(global-set-key (kbd "M-m m f o") 'org-fold-show-subtree)
(global-set-key (kbd "M-m m f h") 'org-fold-hide-sublevels)
(global-set-key (kbd "M-m m l s") 'org-store-link)
(global-set-key (kbd "M-m m l i") 'org-insert-link)
(global-set-key (kbd "M-m m l o") 'org-open-at-point)
(global-set-key (kbd "M-m m v h") 'boxy-headings)
(global-set-key (kbd "M-m m h b") 'org-hide-block-all)

(setq org-confirm-babel-evaluate nil)

(beacon-mode 1)

(defun quelpa-slurp-file (file)
  "Return the contents of FILE as a string, or nil if no such
file exists."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq-local buffer-file-coding-system 'binary)
      (insert-file-contents-literally file)
      (buffer-substring-no-properties (point-min) (point-max)))))


(package-install 'websocket)
;; (add-to-list 'load-path "path/to/org-ai")
(require 'org)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-model "gpt-4-turbo") ; if you are on the gpt-4 beta:
(org-ai-install-yasnippets)
;(setq org-ai-openai-api-token "")

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/my/setup/emacs/org-roam/"))
  (org-roam-completion-everywhere t)
  :bind (("M-m a o t" . org-roam-buffer-toggle)
         ("M-m a o f" . org-roam-node-find)
         ("M-m a o g" . org-roam-graph)
         ("M-m a o i" . org-roam-node-insert)
         ("M-m a o c" . org-roam-capture)
         ;; Dailies
         ("M-m a o r j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(setq org-roam-v2-ack t)

;; Hide empty sections in agenda view
(defun org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)


(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
;; END - Hide empty sections in agenda view

(setq org-directory "~/my/setup/emacs/org-roam/"
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      ;; org-ellipsis " ▼ "
      ;; org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
      ;; org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
      org-log-done 'time
      org-table-convert-region-max-lines 20000
      org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
      '((sequence
         "TODO(t)"           ; A task that is ready to be tackled
         "EPIC(e)"           ; Epics
         "PLAN(p)"           ; plans
         "ANALYTIC(a)"       ; plans
         "INPROGRESS(i)"     ; A task that is ready to be tackled
         "BLOCKED(b)"        ; HOWTO
         "NEXT(n)"           ; HOWTO
         "HIGH(h)"           ; HOWTO
         "TRACK(r)"           ; HOWTO
         "STATUS(s)"         ; HOWTO
         "RETRO(o)"         ; HOWTO
         "UNSORTED(o)"         ; HOWTO
         "|"                 ; The pipe necessary to separate "active" states and "inactive" states
         "DONE(d)"           ; Task has been completed
         "CANCELLED(c)" ))) ; Task has been cancelled
(setq org-agenda-block-separator 45)

(setq org-todo-keyword-faces
      '(("RETRO" . "orange") ("UNSORTED" . "magenta") ("BLOCKED" . "red") ("DONE" . "green"))
      )
;; Agenda files
(setq org-agenda-files
      (append
       (file-expand-wildcards "/home/mshogin/my/setup/emacs/org-roam/*.org")
       )
      )


;; People
(defvar tag-to-header-alist
  '(
    ("kim" . "Kim")
    ("sereja" . "Sereja")
    ("valya" . "Marchuk")
    ("larisa" . "Larisa")
    ("varya" . "Varvara")
    ("igor" . "Igor")
    ("pasha" . "Pavel")
    ("nur" . "Nurbek")
    ("stas" . "Stas")
    ("roma" . "Roma")
    ("yar" . "Yaroslav")
    ("vansevich" . "Vansevich")
    ("piskunov" . "Piskunov")
    ))

(defun prot-create-org-agenda-tag-item (tag header)
  `(tags ,tag
         (
          (org-agenda-overriding-header ,header))))

(defun prot-create-org-agenda-items (tag-header-alist)
  (mapcar (lambda (item)
            (prot-create-org-agenda-tag-item (car item) (cdr item)))
          tag-header-alist))

(defvar my-org-agenda-people
  `((,@(prot-create-org-agenda-items tag-to-header-alist))))


;; ADR
(defvar my-org-agenda-adr
  `(
    ((tags "adr"
           ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            (org-agenda-overriding-header "ADR"))))
    ))


;; (defvar my-org-agenda-hrtech
;;   `((
;;           (agenda "" ((org-agenda-span 'day) (org-agenda-overriding-header "Must DONE")))
;;           (tags "status" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Check Status ")))
;;           (tags-todo "tb" ((org-agenda-overriding-header "交易机器人 (Jiāoyì jīqìrén)") (org-agenda-max-entries 5)))
;;           (tags-todo "team"((org-agenda-overriding-header "Team") (org-agenda-max-entries 5)))
;;           (tags-todo "ats"((org-agenda-overriding-header "ATS") (org-agenda-max-entries 5)))
;;           (tags-todo "devops"((org-agenda-overriding-header "DevOps") (org-agenda-max-entries 5)))
;;           (tags-todo "auth"((org-agenda-overriding-header "Auth (top10)") (org-agenda-max-entries 10)))
;;           (tags-todo "pd" ((org-agenda-overriding-header "Personal Data (top5)") (org-agenda-max-entries 5)))
;;           (tags-todo "cr"((org-agenda-overriding-header "克里特欧" (org-agenda-max-entries 5))))
;;           (tags "backlog" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Backlog")))
;;           (agenda "")

;;      )))

(defvar my-org-agenda-hrtech
  `((
     (tags-todo "pd" ((org-agenda-overriding-header "Personal Data (top5)") (org-agenda-max-entries 5)))
     (tags-todo "ats"((org-agenda-overriding-header "ATS") (org-agenda-max-entries 5)))
     (tags-todo "hrportal"((org-agenda-overriding-header "HRPortal") (org-agenda-max-entries 5)))
     (tags-todo "wbteam"((org-agenda-overriding-header "WBTeam") (org-agenda-max-entries 5)))
     (tags-todo "candb"((org-agenda-overriding-header "C&B") (org-agenda-max-entries 5)))
     (tags-todo "infra"((org-agenda-overriding-header "Infra") (org-agenda-max-entries 5)))
     (tags-todo "devops"((org-agenda-overriding-header "DevOps") (org-agenda-max-entries 5)))
     (tags-todo "auth"((org-agenda-overriding-header "Auth (top10)") (org-agenda-max-entries 5)))
     (tags "hrbacklog" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Backlog")))

     )))


(defun skip-tagged-entry ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (member "skip" (org-get-tags))
        next-headline
      nil)))

;; Unsorted
(defvar my-org-agenda-unsorted
  `(
    ((tags "-TODO=\"TODO\"-TODO=\"DONE\"-TODO=\"EPIC\"-TODO=\"PLAN\""
           ((org-agenda-overriding-header "Non-TODO entries")
            (org-agenda-skip-function '(skip-tagged-entry)))))
    ))

;; EPICS
(defvar my-org-agenda-epic
  `(
    ((tags "+hrtech+TODO=\"EPIC\"+TODO=\"TODO\""
           ((org-agenda-overriding-header "Epics")
            (org-agenda-skip-function '(skip-tagged-entry)))))
    ))



;; (defvar my-org-agenda-hrtech
;;   `((
;;           (agenda "" ((org-agenda-span 'day) (org-agenda-overriding-header "Must DONE")))
;;           (tags "status" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Check Status ")))
;;           (tags-todo "tb" ((org-agenda-overriding-header "交易机器人 (Jiāoyì jīqìrén)") (org-agenda-max-entries 5)))
;;           (tags-todo "team"((org-agenda-overriding-header "Team") (org-agenda-max-entries 5)))
;;           (tags-todo "ats"((org-agenda-overriding-header "ATS") (org-agenda-max-entries 5)))
;;           (tags-todo "devops"((org-agenda-overriding-header "DevOps") (org-agenda-max-entries 5)))
;;           (tags-todo "auth"((org-agenda-overriding-header "Auth (top10)") (org-agenda-max-entries 10)))
;;           (tags-todo "pd" ((org-agenda-overriding-header "Personal Data (top5)") (org-agenda-max-entries 5)))
;;           (tags-todo "cr"((org-agenda-overriding-header "克里特欧" (org-agenda-max-entries 5))))
;;           (tags "backlog" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Backlog")))
;;           (agenda "")

;;      )))

(defvar my-org-agenda-tb
  `((
     (tags-todo "tb" ((org-agenda-overriding-header "交易机器人 (Jiāoyì jīqìrén)") (org-agenda-max-entries 5)))
     )))

(defvar my-org-agenda-cr
  `((
     (tags-todo "cr"((org-agenda-overriding-header "克里特欧" (org-agenda-max-entries 5))))
     )))

(setq org-agenda-span 30)

;; Personal
(defvar my-org-agenda-personal
  `(
    (
     (tags-todo "+personal+high+TODO=\"TODO\""
                ((org-agenda-overriding-header "!!!!!!!!!!!!!!!!!HIGH!!!!!!!!!!!!!!!!!!!!!!!")))
     (tags-todo "+personal+TODO=\"EPIC\""
                ((org-agenda-overriding-header "EPICs")))
     (tags-todo "+mission+TODO=\"TODO\""
                ((org-agenda-overriding-header "Mission")))
     (tags-todo "+brand+TODO=\"TODO\""
                ((org-agenda-overriding-header "Brand")))
     (tags-todo "+mentor+TODO=\"TODO\""
                ((org-agenda-overriding-header "Mentorship")))
     (tags "+org+TODO=\"TODO\""
           ((org-agenda-overriding-header "Org")))
     (tags "personal"
           ((org-agenda-overriding-header "All")))
     )
    ))

(defvar my-org-agenda-hrtech-infra
  `(
    (
     (tags-todo "+infra+TODO=\"EPIC\""
                ((org-agenda-overriding-header "EPICs")))
     (tags-todo "+auth+TODO=\"TODO\""
                ((org-agenda-overriding-header "Auth")))
     (tags-todo "+pd+TODO=\"TODO\""
                ((org-agenda-overriding-header "Personal Data")))
     (tags-todo "+podpis+TODO=\"TODO\""
                ((org-agenda-overriding-header "Billing Podpis")))
     (tags-todo "+search+TODO=\"TODO\""
                ((org-agenda-overriding-header "Search")))
     (tags "+tokens+TODO=\"TODO\""
           ((org-agenda-overriding-header "Tokens review")))
     (tags "+infra-auth-pd-podpis-search-tokens"
           ((org-agenda-overriding-header "All tasks")))
     )
    ))

(setq org-agenda-sorting-strategy
      '((priority-down todo-state-down category-keep)
        (priority-down category-keep)
        (priority-down)
        (todo-state-down)
        (scheduled-up)
        (scheduled deadline)
        (priority-up)))

(setq org-agenda-custom-commands
      `(
        ("i" "HRTech.Infra" ,@my-org-agenda-hrtech-infra)
        ("w" "HRTech.WBTeam" (
                              (tags-todo "+wbteam+TODO=\"EPIC\"" ((org-agenda-overriding-header "EPICs")))
                              (tags-todo "+wbteam-TODO=\"EPIC\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "Tasks")))
                              (tags "+wbteam-TODO=\"EPIC\"-TODO=\"TODO\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "All tasks")))
                              ))
        ("n" "HRTech.C&B" (
                           (tags-todo "+candb+TODO=\"EPIC\"" ((org-agenda-overriding-header "EPICs")))
                           (tags-todo "+candb-TODO=\"EPIC\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "Tasks")))
                           (tags "+candb-TODO=\"EPIC\"-TODO=\"TODO\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "All tasks")))
                           ))
        ("z" "HRTech.ATS" (
                           (tags-todo "+ats+TODO=\"EPIC\"" ((org-agenda-overriding-header "EPICs")))
                           (tags-todo "+ats-TODO=\"EPIC\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "Tasks")))
                           (tags "+ats-TODO=\"EPIC\"-TODO=\"TODO\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "All tasks")))
                           ))
        ("b" "交易机器人" ,@my-org-agenda-tb)
        ("u" "Unsorted" ,@my-org-agenda-unsorted)
        ("t" "team" ,@my-org-agenda-people)
        ("r" "ADR" ,@my-org-agenda-adr)
        ("c" "克里特欧" ,@my-org-agenda-cr)
        ("p" "Personal" (
                         (tags-todo "+personal+high+TODO=\"TODO\"" ((org-agenda-overriding-header "!!!!!!!!!!!!!!!!!HIGH!!!!!!!!!!!!!!!!!!!!!!!")))
                         (tags-todo "+family+TODO=\"TODO\"" ((org-agenda-overriding-header "Family") (org-agenda-max-entries 2)))
                         (tags-todo "+mission+TODO=\"TODO\"" ((org-agenda-overriding-header "Mission") (org-agenda-max-entries 2)))
                         (tags-todo "+brand+TODO=\"TODO\"" ((org-agenda-overriding-header "Brand") (org-agenda-max-entries 2)))
                         (tags-todo "+mentor+TODO=\"TODO\"" ((org-agenda-overriding-header "Mentorship") (org-agenda-max-entries 2)))
                         (tags-todo "+togaf+TODO=\"TODO\"" ((org-agenda-overriding-header "Togaf certificate") (org-agenda-max-entries 2)))
                         (tags-todo "+tb+TODO=\"TODO\"" ((org-agenda-overriding-header "Trading bot") (org-agenda-max-entries 2)))
                         (tags "+org+TODO=\"TODO\"" ((org-agenda-overriding-header "Org") (org-agenda-max-entries 2)))
                         (tags "personal" ((org-agenda-overriding-header "All top10") (org-agenda-max-entries 10)))
                         (tags-todo "+personal+TODO=\"EPIC\"" ((org-agenda-overriding-header "EPICs")))
                         ))
        ("o" "HRTech.HRPortal" (
                                (tags-todo "+hrportal+TODO=\"EPIC\"" ((org-agenda-overriding-header "EPICs")))
                                (tags-todo "+hrportal-TODO=\"EPIC\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "Tasks")))
                                (tags "+hrportal-TODO=\"EPIC\"-TODO=\"TODO\"-TODO=\"PLAN\"-TODO=\"DONE\"" ((org-agenda-overriding-header "All tasks")))
                                ))
        ("e" "EPICs" (
                      (tags "+hrtech+TODO=\"EPIC\"" ((org-agenda-overriding-header "Epics")))
                      (tags "+hrtech+TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")))
                      ))
        ("h" "HRTech" (
                       (tags "+mshogin+TODO=\"TODO\""(
                                                     (org-agenda-sorting-strategy '(priority-down deadline-up scheduled-up effort-down alpha-up))
                                                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                                     (org-agenda-overriding-header "mshogin")
                                                     ))
                       (tags-todo "+infra+control+TODO=\"EPIC\""((org-agenda-overriding-header "PulseCheck: Infra") ))
                       (tags-todo "+devops+control+TODO=\"EPIC\""((org-agenda-overriding-header "PulseCheck: DevOps") ))
                       (tags-todo "+hrportal+control+TODO=\"EPIC\""((org-agenda-overriding-header "PulseCheck: HRPortal") ))
                       (tags-todo "+wbteam+control+TODO=\"EPIC\""((org-agenda-overriding-header "PulseCheck: WBTeam") ))
                       (tags-todo "+hrtech+people+control+TODO=\"TODO\""((org-agenda-overriding-header "PulseCheck: People") (org-agenda-max-entries 10)))
                       ;; (tags-todo "analytic"((org-agenda-max-entries 10)(org-agenda-overriding-header "Status: HR Analytic") ))
                       ;; (tags-todo "arch" ((org-agenda-overriding-header "Status: Arch") (org-agenda-max-entries 10) ))
                       ;; (tags-todo "pd" ((org-agenda-overriding-header "Status: Personal Data (top10)") (org-agenda-max-entries 10) ))
                       ;; (tags-todo "ats"((org-agenda-overriding-header "Status: ATS") (org-agenda-max-entries 10)))
                       ;; (tags-todo "hrportal"((org-agenda-overriding-header "Status: HRPortal") (org-agenda-max-entries 10)))
                       ;; (tags-todo "wbteam"((org-agenda-overriding-header "Status: WBTeam") (org-agenda-max-entries 10)))
                       ;; (tags-todo "candb"((org-agenda-overriding-header "Status: C&B") (org-agenda-max-entries 10)))
                       ;; (tags-todo "devops"((org-agenda-overriding-header "Status: DevOps") (org-agenda-max-entries 10)))
                       ;; (tags-todo "auth"((org-agenda-overriding-header "Status: Auth (top10)") (org-agenda-max-entries 10)))
                       ;; (tags-todo "dochub"((org-agenda-overriding-header "Status: DocHub") (org-agenda-max-entries 10)))
                       ;; (tags-todo "integrations"((org-agenda-overriding-header "Status: Integrations") (org-agenda-max-entries 10)))
                       ;; (tags-todo "+howto+TODO=\"HOWTO\"" ((org-agenda-overriding-header "Status: HowTo") (org-agenda-max-entries 10)))
                       ;; (tags "hrbacklog" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)) (org-agenda-overriding-header "Status: Backlog")))
                       ))

        ))

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert)
  )

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t))
          t)

(org-auto-tangle-mode)
;; 1ISo?W5fVId!UEUw$9>!


;; (password-store-insert "foo-account" "password")
;; (password-store-get "foo-account")
;; (password-store-get-field "foo-account" "url") ; Returns "url"

;; (require 'ob-html-chrome)
;; (setq org-babel-html-chrome-chrome-executable
;;       "/usr/bin/google-chrome")
