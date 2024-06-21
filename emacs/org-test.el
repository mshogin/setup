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
    ("roma" . "Roma")
    ("yar" . "Yaroslav")
    ("piskunov" . "Piskunov")
    ))

(setq org-agenda-block-separator 45)
(defun prot-create-org-agenda-tag-item (tag header)
  `(tags ,tag
         (
          (org-agenda-overriding-header ,header))))

(defun prot-create-org-agenda-items (tag-header-alist)
  (mapcar (lambda (item)
            (prot-create-org-agenda-tag-item (car item) (cdr item)))
          tag-header-alist))

(defvar prot-org-custom-daily-agenda
  `((,@(prot-create-org-agenda-items tag-to-header-alist))))

(setq org-agenda-custom-commands
      `(("p" "People"
         ,@prot-org-custom-daily-agenda)))



(("p" "People" ((tags "marchuk" ((org-agenda-block-separator nil) (org-agenda-overriding-header "Marchuk"))) (tags "kim" ((org-agenda-block-separator nil) (org-agenda-overriding-header "Kim"))))))


(defvar prot-org-custom-daily-agenda
  `((
   ,(prot-create-org-agenda-tag-item "marchuk" "Marchuk")
   ,(prot-create-org-agenda-tag-item "kim" "Kim")
   )))

(setq org-agenda-custom-commands
      `(("p" "People"
         ,@prot-org-custom-daily-agenda
         )))

(setq org-agenda-custom-commands
      '(

        ("p" "People"
         (
          (tags "marchuk" ((org-agenda-block-separator nil) (org-agenda-overriding-header "Marchuk")))
          (tags "kim" ((org-agenda-block-separator nil) (org-agenda-overriding-header "Kim")))
         )

        )))






;; (defvar prot-org-custom-daily-agenda
;;   `(
;;     (tags "kim"
;;           (
;;            (org-agenda-block-separator nil)
;;            (org-agenda-overriding-header "Kim")
;;            )
;;           )

;;     ))

;; (setq org-agenda-custom-commands
;;       `(("a" "People"
;;          ,prot-org-custom-daily-agenda)
;;         )
;;       )
