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

;; (setq doom-font (font-spec :family "MesloLGSDZ Nerd Font" :size 14.0)
;;       doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16.0))

(cond ((eq system-type 'gnu/linux)
        (setq doom-font (font-spec :family "SauceCodePro NF" :size 14.0)
            doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 18.0 :weight 'regular)
            doom-serif-font (font-spec :family "DejaVu Serif" :size 16.0)
            doom-big-font (font-spec :size 28.0))
       )
      ((eq system-type 'darwin)
        (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 16.0)
            doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 18.0 :weight 'regular)
            doom-serif-font (font-spec :family "PT Serif" :size 16.0)
            doom-big-font (font-spec :size 28.0))
       ))

(setq doom-theme 'doom-vibrant)

(setq-default line-spacing 0.1)

(setq +zen-text-scale 0.7)

(setq  evil-want-fine-undo t
       undo-limit 80000000)

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit evil-window-new)
  (persp-switch-to-buffer))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(after! evil
  (evil-select-search-module 'evil-search-module 'isearch))

(define-key evil-normal-state-map (kbd "go") 'counsel-outline)

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?t ?e ?i ?s ?r ?o ?a ?n)))

(setq org-directory "~/notes/")

(add-hook! org-mode
           #'org-modern-mode)
(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)

(defcustom org-modern-star '("◉" "○" "◌" "⁖" "◿")
        "Overwrite org-modern's provided heading stars."
        :type '(repeat string))

;; Add frame borders and window dividers
;; (after! org
;;     (modify-all-frames-parameters
;;     '((right-divider-width . 10)
;;     (internal-border-width . 10)))
;;     (dolist (face '(window-divider
;;                     window-divider-first-pixel
;;                     window-divider-last-pixel))
;;     (face-spec-reset-face face)
;;     (set-face-foreground face (face-attribute 'default :background)))
;;     (set-face-background 'fringe (face-attribute 'default :background))
;; )

(use-package! mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; (custom-set-faces! '(variable-pitch :height 160))

(custom-set-faces!
  '(variable-pitch :family "Source Sans Pro" :height 140 :weight regular))

(after! org
  (setq
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " ▾ "
   ;; From minad/org-modern: Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-indent-indentation-per-level 2

   org-agenda-files '("~/notes/tasks.org"
                      "~/notes/cs61b_syllabus.org")
   org-startup-folded 'content
   )

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

(custom-set-faces! '(org-quote :inherit doom-variable-pitch-font :slant normal))

(custom-set-faces! '(fixed-pitch :inherit doom-font :size 14))

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

(setq org-agenda-files '("~/notes/tasks.org"))

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

(use-package! org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes")
  (org-roam-capture-templates
   '(("d" "default" plain
      "#+filetags: %?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))

  (org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-setup))

(map! :leader
      (:prefix-map ("r" . "Org-Roam commands")
       :desc "Toggle org-roam buffer"
       "t" #'org-roam-buffer-toggle
       :desc "Find or Create Node"
       "f" #'org-roam-node-find
       :desc "Insert Node"
       "i" #'org-roam-node-insert
       :desc "Create id for heading node"
       "c" #'org-id-get-create
       :desc "Add alias for node"
       "a" #'org-roam-alias-add
       )
      )

(setq company-global-modes '(not org-mode))

(setq fill-column 90)

(defun center-visual-fill ()
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'visual-line-mode-hook #'center-visual-fill)

(map! :leader
      :desc "visual-fill-column-mode"
      "W" #'visual-fill-column-mode)

(defun open-task-file ()
  "Open tasks.org file."
  (interactive)
  (find-file-existing "~/notes/tasks.org"))
(global-set-key (kbd "C-c t") 'open-task-file)

(defun open-hours-log ()
  "Open hours-log.org file."
  (interactive)
  (find-file-existing "~/notes/hours-log.org"))
(global-set-key (kbd "C-c h") 'open-hours-log)

(map! :leader
      :desc "Toggle narrow subtree"
      "t n" #'org-toggle-narrow-to-subtree)

(map! :after evil-org :map evil-org-mode-map
      :nv "z o" #'evil-open-fold)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs-from-scratch/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook! org-mode (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
