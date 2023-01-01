(setq org-agenda-files '("~/notes/tasks.org"
                         "~/notes/cs61b_syllabus.org"))

(after! org

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANCELLED(k@)")))

   ;; Agenda styling
   (setq
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

 ;;    (setq org-todo-keyword-faces
 ;;      '(("TODO" . (:foreground "#FB4934" :weight regular))
 ;;        ("NEXT" . (:foreground "#458588" :slant italic))
 ;;        ("ACTIVE" . (:foreground "#076678" :slant italic))
 ;;        ("DONE" . (:foreground "#8EC07C" :weight light :strike-through t))
 ;;        ("READ" . (:foreground "#b16286" :weight regular))
 ;;        ("READING" . (:foreground "#8f3f71" :weight regular))
 ;;        ("WAITING" . (:foreground "black" :weight light))
 ;;   )
 ;; )
)

(defun fw/agenda-icon-octicon (name)
  "Returns an all-the-icons-octicon icon"
  (list (all-the-icons-octicon name)))

(defun fw/agenda-icon-faicon (name)
  "Returns an all-the-icons-faicon icon"
  (list (all-the-icons-faicon name)))

;; The strings listed first ("Postdoc", etc.) refer to the categories under headings in my tasks.org file.
;; https://old.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
(setq org-agenda-category-icon-alist
      `(("Postdoc" ,(fw/agenda-icon-octicon "pencil") nil nil :ascent center)
        ("Coding" ,(fw/agenda-icon-faicon "code") nil nil :ascent center)
        ("FRG" ,(fw/agenda-icon-octicon "book") nil nil :ascent center)
        ("Home" ,(fw/agenda-icon-octicon "home") nil nil :ascent center)
        ("Habits" ,(fw/agenda-icon-faicon "calendar-check-o") nil nil :ascent center)
        ))

(setq org-agenda-custom-commands
  '(
    ("n" "Active and Next Tasks"
     (
        (todo "ACTIVE"
                ((org-agenda-overriding-header "\nActive Tasks\n-----------------")
                (org-agenda-prefix-format "   %i %?-2 t%s")
                (org-agenda-remove-tags t)))
        (todo "NEXT"
                ((org-agenda-overriding-header "\nNext Tasks\n----------")
                (org-agenda-prefix-format "   %i %?-2 t%s")
                (org-agenda-remove-tags t)))
        (agenda "" (
                (org-deadline-warning-days 8)
                (org-agenda-remove-tags t)
                (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                (org-agenda-overriding-header "\nSchedule\n--------")))))

    ("h" "Home-related tasks"
       (tags-todo "home"
        ((org-agenda-overriding-header "Home Tasks")
        (org-agenda-remove-tags t)
        ))
     )

    ("w" "Work-related tasks"
     (
      (tags-todo "+postdoc-jobs"
        ((org-agenda-overriding-header "\nPostdoc Tasks")))
      (tags-todo "book"
        ((org-agenda-overriding-header "\nBook Tasks")))
      (tags-todo "jobs"
        ((org-agenda-overriding-header "\nJob Application Tasks")))
     ))

    ("b" "Book-related tasks"
     ( tags-todo "book"
        ((org-agenda-overriding-header "\nBook Tasks")
        (org-agenda-remove-tags t)
        )
     ))

    ;; ("r" "Reading Tasks"
    ;;  ((todo "READING"
    ;;     ((org-agenda-overriding-header "\nCurrently Reading")
    ;;     (org-agenda-remove-tags t)
    ;;     ))
    ;;   (todo "READ"
    ;;     ((org-agenda-overriding-header "\nTo Read")
    ;;     (org-agenda-remove-tags t)
    ;;     ))
    ;;   ))
))
