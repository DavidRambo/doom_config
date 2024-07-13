(add-to-list 'exec-path "~/.nix-profile/bin")

(setq user-full-name "David Rambo"
      user-mail-address "davidrambo@mailfence.com")

(setq initial-frame-alist '((top . 120) (left . 1262) (width . 92) (height . 50)))

(defun frame-center ()
  "Center the current frame."
  (interactive)
  (let* ((dw (display-pixel-width))
         (dh (display-pixel-height))
         (f  (selected-frame))
         (fw (frame-pixel-width f))
         (fh (frame-pixel-height f))
         (x  (- (/ dw 2) (/ fw 2)))
         (y  (- (/ dh 2) (/ fh 2))))
    (message (format "dw %d dh %d fw %d fh %d x %d y %d" dw dh fw fh x y))
    (set-frame-position f x y)))

(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(undecorated . t)))
      ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(undecorated-round . t))))

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

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(cond ((eq system-type 'gnu/linux)
        ;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13.0)
        (setq doom-font (font-spec :family "IosevkaTerm Nerd Font" :size 14.0)
            doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14.0 :weight 'regular)
            doom-serif-font (font-spec :family "Palatino Linotype" :size 16.0)
            doom-big-font (font-spec :size 28.0))
       )
      ((eq system-type 'darwin)
        (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 14.0 :weight 'regular)
            doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16.0 :weight 'regular)
            doom-serif-font (font-spec :family "PT Serif" :size 16.0)
            doom-big-font (font-spec :size 28.0))
       ))

(set-face-attribute 'italic nil
                    :slant 'italic
                    :underline nil)

(setq doom-theme 'catppuccin
      catppuccin-flavor 'latte
      catppuccin-enlarge-headings 'nil)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(setq +zen-text-scale 0.7)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq  evil-want-fine-undo t
       undo-limit 80000000)

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit evil-window-new)
  (persp-switch-to-buffer))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(map! :after evil-org :map evil-org-mode-map
      :leader
      (:nv
       "w u" #'evil-window-up
       ;:desc "Find or Create Node"
       "w e" #'evil-window-down
       ;:desc "Insert Node"
       "w i" #'evil-window-right
       ;:desc "Create id for heading node"
       "w n" #'evil-window-left
       ;:desc "evil-window-split"
       "w s" #'evil-window-split
       ;:desc "evil-window-split"
       "w v" #'evil-window-vsplit
       ;:desc "evil-window-delete"
       "w c" #'evil-window-delete
       ))

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "#e64553"))
(setq doom-modeline-height 28)

(after! evil
  (evil-select-search-module 'evil-search-module 'isearch))

(define-key evil-normal-state-map (kbd "go") 'counsel-outline)

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?t ?e ?i ?s ?r ?o ?a ?n)))

(setq org-directory "~/notes/")

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "●" "○" "⁖" "◿" "◌"))
  )

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
      (setq variable-pitch (font-spec :family "Iosevka Aile"))
      (cond ((eq system-type 'gnu/linux)
            (set-face-attribute 'variable-pitch nil :height 140)
             )
            ((eq system-type 'darwin)
            (set-face-attribute 'variable-pitch nil :height 160)
             )
        )

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
  (cond ((eq system-type 'gnu/linux)
        (setq variable-pitch-serif-font (font-spec :family "Palatino Linotype" :size 16.0))
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

(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                             (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))
)

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
   org-startup-folded 'content
   )

  (set-face-attribute 'org-document-title nil :weight 'light :height 1.1)

   ;; Heading Styles
   (dolist (face
            '((org-level-1 . 1.1)
              (org-level-2 . 1.05)
              (org-level-3 . 1.0)
              (org-level-4 . 1.0)
              (org-level-5 . 1.0)
              (org-level-6 . 1.0)
              (org-level-7 . 1.0)
              (org-level-8 . 1.0)))
   (set-face-attribute (car face) nil :weight 'thin :height (cdr face)))
)

(custom-set-faces!
  '(org-quote :inherit doom-variable-pitch-font :slant normal))
(setq org-fontify-whole-block-delimiter-line nil)

(after! org
  (custom-set-faces!
  ; Macchiatto --> '(org-block :background "#1e2030")) ; for Macchiato
    '(org-block :background "#e6e9ef"))) ; for Latte

(add-hook! 'org-mode #'org-appear-mode)

(setq org-ascii-bullets '((ascii ?* ?+ ?-) (latin1 ?* ?+ ?-) (utf-8 ?* ?+ ?-)))

(use-package! ox-moderncv
    :init
    (require 'ox-moderncv))

(after! org
  (use-package! ox-latex
    :init
    ;; this code runs immediately
    :config
    ;; this code runs after the package loads
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stops org from adding hypersetup{}

    ;; delete unwanted files
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
    (unless (boundp 'org-latex-classes)
        (setq org-latex-classes nil)))
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))

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
    org-agenda-todo-keyword-format ""
    org-agenda-tags-column 0
    org-agenda-block-separator ?─
    org-agenda-time-grid
    '((daily today require-timed)
      (800 1000 1200 1400 1600 1800 2000)
      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    org-agenda-current-time-string
    "⭠ now ─────────────────────────────────────────────────"

    org-modern-todo-faces
    '(("TODO" . (:foreground "#1c1f24" :background "#ee99a0" :weight regular))
      ("NEXT" . (:foreground "#1c1f24" :background "#eed49f" :slant italic))
      ("ACTIVE" . (:foreground "#1c1f24" :background "#a6da95" :slant italic))
      ("DONE" . (:foreground "#1c1f24" :background "#91d7e3" :weight light :strike-through t))
        ("WAIT" . (:foreground "#1c1f24" :background "#b7bdf8" :weight light)))))
 ;;      ("READ" . (:foreground "#b16286" :weight regular))
 ;;      ("READING" . (:foreground "#8f3f71" :weight regular))
 ;;      ("WAITING" . (:foreground "black" :weight light))))

(setq org-agenda-category-icon-alist
      `(("Postdoc" ,(nerd-icons-octicon "nf-oct-pencil") nil nil :ascent center)
        ("Coding" ,(nerd-icons-faicon "nf-fa-code") nil nil :ascent center)
        ("Home" ,(nerd-icons-octicon "nf-oct-home") nil nil :ascent center)
        ("Habits" ,(nerd-icons-faicon "nf-fa-calendar_check_o") nil nil :ascent center)
        ))

(setq org-agenda-custom-commands
  '(("n" "Active and Next Tasks"
     ((todo "ACTIVE"
            ((org-agenda-overriding-header "\nActive Tasks\n-----------------")
             (org-agenda-prefix-format "   %i %?-2 t%s")
             (org-agenda-remove-tags nil)))
      (todo "NEXT"
            ((org-agenda-overriding-header "\nNext Tasks\n-----------------")
             (org-agenda-prefix-format "   %i %?-2 t%s")
             (org-agenda-remove-tags nil)))
      (agenda ""
            ((org-deadline-warning-days 8)
             (org-agenda-remove-tags t)
             (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
             (org-agenda-overriding-header "\nSchedule\n-----------------")))))

    ("h" "Home-related tasks"
       (tags-todo "home"
        ((org-agenda-overriding-header "Home Tasks")
        (org-agenda-remove-tags t)
        ))
     )

    ("o" "OSU"
     ((tags-todo "+OSU"
                ((org-agenda-overriding-header "OSU")
                 (org-agenda-remove-tags nil)))))

    ("w" "Work-related tasks"
     (
      (tags-todo "jobs"
        ((org-agenda-overriding-header "\nJob Application Tasks")))
      (tags-todo "+coding"
        ((org-agenda-overriding-header "\nProgramming Tasks")))
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

  ;; directory is relative to org-roam-directory
  (org-roam-dailies-directory "../journal/")

  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>\n%?"
     :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%B %d, %Y (%A)>\n")
     :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%B %d, %Y (%A)>\n* Sleep Diary - %<%I:%M %p>\n1. ")
     ;; :unnarrowed t
     )
    ;; ("s" "sleep diary" entry "* Sleep Diary - %<%I:%M %p>\n1. %?"
    ;;  :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
    )
   )

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
       :desc "Dailies capture map"
       "d" #'org-roam-dailies-map
       :desc "Capture daily journal"
       "j" #'org-roam-dailies-capture-today
       )
      )

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq
        org-log-done nil
        org-agenda-start-day nil
        org-agenda-span 10
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator 9472
        org-agenda-tags-column 60
        org-agenda-compact-blocks nil
        org-agenda-dim-blocked-tasks t
        org-agenda-start-on-weekday nil
        org-super-agenda-groups nil
        )
  (org-super-agenda-mode)
)

(after! git-gutter
  (setq git-gutter:disabled-modes '(org-mode image-mode)))

(setq company-global-modes '(not org-mode))

(defun center-visual-fill ()
  (setq fill-column 80)
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
  (find-file-existing "~/notes/time_ledger.timedot")
  (org-mode))
(global-set-key (kbd "C-c h") 'open-hours-log)

(map! :leader
      :desc "Toggle narrow subtree"
      "t n" #'org-toggle-narrow-to-subtree)

(map! :after evil-org :map evil-org-mode-map
      :nv "z o" #'evil-open-fold)

(use-package! powerthesaurus
  :defer t)
