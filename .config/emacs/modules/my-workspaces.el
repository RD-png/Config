;;; my-workspaces.el -*- lexical-binding: t; -*-

(use-package tab-bar
  :ensure nil
  :bind (("C-x C-'" . tab-bar-switch-to-recent-tab))
  :config
  (setq tab-bar-show nil))

(use-package tab-bar-echo-area
  :ensure t
  :disabled t
  :after tab-bar
  :init
  (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode")
  :config
  (tab-bar-echo-area-mode 1))

(use-package tabspaces
  :ensure t
  :disabled t
  :hook (after-init . tabspaces-mode)
  :bind (("C-c s s" . tabspaces-switch-or-create-workspace)
         ("C-c s k" . tabspaces-close-workspace)
         ("C-c s K" . tabspaces-kill-buffers-close-workspace)
         ("C-c s c" . tabspaces-clear-buffers)
         ("C-c s b" . tabspaces-switch-to-buffer)
         ("C-c s p" . tabspaces-open-or-create-project-and-workspace)
         ("C-c s r" . tabspaces-restore-session))
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(use-package desktop
  :ensure nil
  :config
  (setq desktop-auto-save-timeout 120
        desktop-save t
        desktop-load-locked-desktop t)
  :init
  (desktop-save-mode))

(use-package activities
  :ensure t
  :bind (("C-c s c" . activities-new)
         ("C-c s s" . activities-switch)
         ("C-c s k" . activities-kill)
         ("C-c s l" . activities-list)
         ("C-c s r" . activities-rename)
         ("C-c s o" . activities-resume)
         ("C-c s K" . activities-discard))
  :init
  (activities-mode)
  (activities-tabs-mode))

(use-package bufferlo
  :ensure t
  :bind (("C-x k" . bufferlo-kill-buffer)
         ("C-c s b" . bufferlo-find-buffer-switch)
         ("C-c s k" . bufferlo-tab-close-kill-buffers))
  :preface
  (defun bufferlo-kill-buffer (buffer)
    (interactive
     (list
      (let ((lbs (mapcar #'buffer-name (bufferlo-buffer-list))))
        (read-buffer
         "Kill local buffer: " (current-buffer) nil
         (lambda (b) (member (if (stringp b) b (car b)) lbs))))))
    (bufferlo-remove buffer))
  :config
  (defvar my-consult--source-local-buffers
    (list :name "Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items (lambda () (consult--buffer-query
                        :predicate #'bufferlo-local-buffer-p
                        :sort 'visibility
                        :as #'buffer-name))))

  (defvar my-consult--source-all-buffers
    (list :name "All Buffers"
          :narrow   ?a
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items (lambda () (consult--buffer-query
                        :predicate #'bufferlo-non-local-buffer-p
                        :sort 'visibility
                        :as #'buffer-name))))

  (add-to-list 'consult-buffer-sources 'my-consult--source-all-buffers)
  (add-to-list 'consult-buffer-sources 'my-consult--source-local-buffers)
  (setq bufferlo-include-buffer-filters '("^\\*\\Messages" "^\\*Warnings"))

  :init
  (bufferlo-mode 1)
  (tab-line-mode nil))

(provide 'my-workspaces)
