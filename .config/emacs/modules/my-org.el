;;; my-org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/org-new-project ()
  (interactive)
  (insert "* " (read-string "Enter Project Name:"))
  (insert " [%]")
  (save-excursion
    (insert (format (concat "\n"
                            ":PROPERTIES:\n"
                            ":CREATED: %s\n"
                            ":COOKIE_DATA: todo recursive\n"
                            ":ID: %s\n"
                            ":END:\n")
                    (format-time-string (org-time-stamp-format t t))
                    (substring (shell-command-to-string "uuidgen") 0 -1))))
  (org-backward-heading-same-level 0)
  (org-toggle-tag "project" 'on)
  (org-next-visible-heading 1))

;;;###autoload
(defun my/org-new-todo-header ()
  (insert "TODO ")
  (save-excursion
    (insert (format (concat "\n"
                            "DEADLINE: %s SCHEDULED: %s\n"
                            ":PROPERTIES:\n"
                            ":CREATED: %s\n"
                            ":ID: %s\n"
                            ":END:\n")
                    (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date t 'to-time nil))
                    (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date t 'to-time nil))
                    (format-time-string (org-time-stamp-format t t))
                    (substring (shell-command-to-string "uuidgen") 0 -1)))))

;;;###autoload
(defun my/org-new-inline-heading ()
  (interactive)
  (org-insert-heading)
  (my/org-new-todo-header))

;;;###autoload
(defun my/org-new-sub-heading ()
  (interactive)
  (org-insert-subheading (org-current-level))
  (my/org-new-todo-header))

;;;###autoload
(defun org-mode-setup ()
  (org-indent-mode)
  (url-handler-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :ensure nil
  :commands (org-capture org-agenda)
  :hook ((org-mode . org-mode-setup)
         (org-mode . visual-fill-column-mode))
  :config
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-special-ctrl-a/e t
        org-auto-align-tags nil
        org-tags-column 0
        org-hide-emphasis-markers t
        org-insert-heading-respect-content t
        org-ellipsis " ▾"
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.75)
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-files (directory-files-recursively "~/.config/emacs/org/" "\\.org$"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "NOTE(n)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("batch" . ?b)
          ("note" . ?n)
          ("project" . ?p)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/.config/emacs/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))

  (define-key global-map (kbd "C-c j")
              (lambda () (interactive) (org-capture nil "jj")))

  ;; (org-font-setup)
  :bind (:map org-mode-map
              ("C-c o p" . my/org-new-project)
              ("C-c o i" . my/org-new-inline-heading)
              ("C-c o s" . my/org-new-sub-heading)))

(use-package org-modern
  :ensure t
  :config
  (setq line-spacing 0.2)
  (setq org-modern-star '("◉" "✳"))
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-ellipsis "…"

   org-modern-block-fringe nil

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  :init
  (global-org-modern-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.config/emacs/org/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-dailies-capture-today)
         ("C-c n r" . org-roam-dailies-capture-tomorrow)
         ("C-c n y" . org-roam-dailies-capture-yesterday)
         ("C-c n g t" . org-roam-dailies-goto-today)
         ("C-c n g r" . org-roam-dailies-goto-tomorrow)
         ("C-c n g y" . org-roam-dailies-goto-yesterday))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package org-auctex
  :ensure (org-auctex :host github :repo "karthink/org-auctex")
  :disabled t
  :hook (org-mode . org-auctex-mode))

(use-package toc-org
  :ensure t
  :defer 5
  :hook (org-mode . toc-org-mode))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'my-org)
