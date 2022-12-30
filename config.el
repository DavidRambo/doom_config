(setq user-full-name "David Rambo"
      user-mail-address "davidrambo@mailfence.com")

(setq display-line-numbers-type 'relative)

(defun dr/display-line-numbers-hook ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'dr/display-line-numbers-hook)
(add-hook 'text-mode-hook 'dr/display-line-numbers-hook)

(defun dr/disable-line-numbers-hook ()
  (display-line-numbers-mode 0)
  )
(add-hook 'org-mode-hook 'dr/disable-line-numbers-hook)

(setq doom-font (font-spec :family "MesloLGSDZ Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

;; (cond ((eq system-type 'gnu/linux)
;;        (setq doom-font (font-spec :family "MesloLGSDZ Nerd Font" :size 16 :height 160)
;;              doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16 :height 160))
;;        (eq system-type 'darwin)
;;        (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 14 :height 140)
;;              doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 15 :height 150))))

(setq-default line-spacing 0.2)

(setq doom-theme 'doom-vibrant)

(setq  evil-want-fine-undo t
       undo-limit 80000000)

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit evil-window-new)
  (persp-switch-to-buffer))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(define-key evil-normal-state-map (kbd "go") 'counsel-outline)

;; (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2-below)
;; (define-key evil-normal-state-map (kbd "S") 'avy-goto-char-2-above)

(setq org-directory "~/notes/")

;; minad/Org-Modern
(add-hook! org-mode
           #'org-modern-mode)
(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)

(defcustom org-modern-star '("◉" "○" "◌" "⁖" "◿")
        "Overwrite org-modern's provided heading stars."
        :type '(repeat string))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Ways to handle org-mode mixed pitch faces ;;

;; *** 1 ***
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)

;; (defun dr/org-mode-setup ()
;;   (variable-pitch-mode 1)
;;   (set-face-attribute 'variable-pitch nil :height 150)
;;   (hl-line-mode nil)
;;   )
;; (add-hook 'org-mode-hook 'dr/org-mode-setup)


;; *** 2 ***
;; (custom-theme-set-faces
;;         'user
;;                 '(variable-pitch ((t (:family "Source Sans Pro" :height 140 :weight regular))))
;;                 '(fixed-pitch ((t ( :family "MesloLGSDZ Nerd Font" :height 140)))))

;; (add-hook 'org-mode-hook 'variable-pitch-mode)


;; *** 3 ***
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch

      (setq mixed-pitch-set-height t)
      (setq variable-pitch (font-spec :family "Source Sans Pro"))
      (cond ((eq system-type 'gnu/linux)
            (set-face-attribute 'variable-pitch nil :height 140)
             )
            ((eq system-type 'darwin)
            (set-face-attribute 'variable-pitch nil :height 140)
             )
        )
      ;; Sans-Serif
      (defun mixed-pitch-sans-mode (&optional arg)
        "Change the default face of the current buffer to a sans-serif variable pitch."
        (interactive)
        (let ((mixed-pitch-face 'variable-pitch))
          (mixed-pitch-mode (or arg 'toggle))))

  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)

  (setq mixed-pitch-set-height t)

  ;; Serif
  (cond ((eq system-type 'gnu/linux)
        (setq variable-pitch-serif-font (font-spec :family "Palatino Linotype" :size 14.0))
       )
      ((eq system-type 'darwin)
        (setq variable-pitch-serif-font (font-spec :family "Palatino" :size 16.0)))
  )
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)

  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle))))
)


(after! org
  (setq
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " ▾"
   ;; From minad/org-modern: Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   org-agenda-files '("~/notes/tasks.org"
                      "~/notes/cs61b_syllabus.org")
   org-startup-folded 'content
   )

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d!)")
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

   ;; Heading Styles
   (dolist (face
            '((org-level-1 . 1.2)
              (org-level-2 . 1.1)
              (org-level-3 . 1.0)
              (org-level-4 . 1.0)
              (org-level-5 . 1.0)
              (org-level-6 . 1.0)
              (org-level-7 . 1.0)
              (org-level-8 . 1.0)))
   (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
)

;; (use-package! org-superstar-mode
;;   :custom
;;     org-superstar-headline-bullets-list '("◉" "○" "◌" "⁖" "◿")
;;     org-superstar-remove-leading-stars
;;   :hook (org-mode . org-bullets-mode))

;; (use-package! prettify-symbols-mode
;;   :custom
;; ; ; (push '("[ ]" .  "☐") prettify-symbols-alist)
;;   prettify-symbols-alist '(("[ ]" . "☐")
;;                           ("[-]" . "❍")
;;                           ("[X]" . "☑"))
;;   :hook (org-mode . prettify-symbols-mode)
;; )

(use-package! org-journal
  :init
  (setq org-journal-dir "~/journal/"
        org-journal-file-type 'daily
        org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-time-format "%I:%M %p\n"
        org-journal-file-format "%Y-%m-%d.org")

  (setq org-journal-enable-agenda-integration nil)
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
        ))

(setq org-agenda-custom-commands
  '(
    ("n" "Active and Next Tasks"
     (
        (todo "ACTIVE"
                ((org-agenda-overriding-header "\nActive Tasks\n------------")
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
     (
      (tags-todo "book"
        ((org-agenda-overriding-header "\nBook Tasks")
        (org-agenda-remove-tags t)
        ))
     ))

    ("p" "Programming Tasks"
     ( (tags-todo "coding"
                 ((org-agenda-overriding-header "\nPrograming Tasks")))))

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

;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq
;;         org-log-done nil
;;         org-agenda-start-day nil
;;         org-agenda-span 7
;;         org-agenda-skip-scheduled-if-done t
;;         org-agenda-skip-deadline-if-done t
;;         org-agenda-include-deadlines t
;;         org-agenda-block-separator 9472
;;         org-agenda-tags-column 100
;;         org-agenda-compact-blocks nil
;;         org-agenda-dim-blocked-tasks nil
;;         org-agenda-start-on-weekday nil
;;         org-super-agenda-groups nil
;;         )
;;   :config
;;   (org-super-agenda-mode)
;; )

;; Visual Column Mode
(setq fill-column 90)

(defun center-visual-fill ()
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'visual-line-mode-hook #'center-visual-fill)

(map! :leader
      :desc "visual-fill-column-mode"
      "W" #'visual-fill-column-mode)

;; Open Task File
;; Function to open tasks.org and then map key to open.
(defun open-task-file ()
  "Open tasks.org file."
  (interactive)
  (find-file-existing "~/notes/tasks.org"))
(global-set-key (kbd "C-c t") 'open-task-file)

;; ** Open hours log
;; Function to open hours-log.org and then map key to open.
;; #+begin_src elisp
(defun open-hours-log ()
  "Open hours-log.org file."
  (interactive)
  (find-file-existing "~/notes/hours-log.org"))
(global-set-key (kbd "C-c h") 'open-hours-log)
;; #+end_src
