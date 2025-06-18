;;; my-workflow.el -*- lexical-binding: t; -*-

(use-package project
  :ensure nil
  :demand t
  :init
  (global-set-key (kbd "C-c p") project-prefix-map)
  ;; (cl-defgeneric project-root (project) (car project))
  ;; (defun my/project-current-root (&optional dir)
  ;;   (when-let ((project
  ;;               (project-current nil (or dir default-directory))))
  ;;     (project-root project)))
  :bind* (("C-c p f" . project-find-file)
          ("C-c p s r" . project-find-regexp)
          ("C-c p d" . project-dired)
          ("C-c p b" . project-switch-to-buffer)
          ("C-c p r" . project-query-replace-regexp)
          ("C-c p v" . project-vc-dir)
          ("C-c p k" . project-kill-buffers)
          ("C-c p !" . project-shell-command)
          ("C-c p e" . project-eshell)
          ("C-c p g" . consult-ripgrep)))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :ensure t
  :after magit)

(use-package git-gutter
  :ensure t
  :disabled t
  :hook (prog-mode . git-gutter-mode)
  :custom-face
  (git-gutter:added ((t (:background "green4"))))
  (git-gutter:modified ((t (:background "goldenrod3"))))
  (git-gutter:deleted ((t (:background "salmon3"))))
  :config
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  ;; (setq git-gutter:update-interval 10)
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))
  (defun git-gutter:start-of-hunk ()
    "Move to end of current diff hunk"
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        (forward-line lines))))
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
  (setq git-gutter:modified-sign " ")
  :bind
  ("C-c C-v t" . git-gutter-mode)
  ("C-c C-v r" . git-gutter:revert-hunk)
  ("C-c C-v m" . git-gutter:mark-hunk)
  ("C-c C-v n" . git-gutter:next-hunk)
  ("C-c C-v p" . git-gutter:previous-hunk)
  ("C-c C-v s" . git-gutter:stage-hunk)
  ("C-c C-v g" . git-gutter:update-all-windows)
  ("C-c C-v d" . git-gutter:popup-hunk)
  ("C-c C-v e" . git-gutter:end-of-hunk)
  ("C-c C-v a" . git-gutter:start-of-hunk))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-margin-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :bind
  ("C-c C-v t" . global-diff-hl-mode)
  ("C-c C-v r" . diff-hl-revert-hunk)
  ("C-c C-v m" . diff-hl-mark-hunk)
  ("C-c C-v n" . diff-hl-next-hunk)
  ("C-c C-v p" . diff-hl-previous-hunk)
  ("C-c C-v s" . diff-hl-stage-current-hunk)
  ("C-c C-v d" . diff-hl-show-hunk)
  :config
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5
        diff-hl-show-staged-changes nil)
  (setq diff-hl-margin-symbols-alist '((insert . " ")
                                       (delete . " ")
                                       (change . " ")
                                       (unknown . " ")
                                       (ignored . " ")))
  (advice-add 'diff-hl-next-hunk :after
              (defun my/diff-hl-recenter
                  (&optional _) (recenter)))
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  ;; :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :hook (pdf-annot-list-mode . hide-mode-line-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-width))

(use-package saveplace-pdf-view
  :ensure t
  :after pdf-view
  :init
  (save-place-mode 1))

(provide 'my-workflow)
