;;; my-navigation.el -*- lexical-binding: t; -*-

(use-package point-history
  :ensure (point-history :host github :repo "blue0513/point-history")
  :bind
  ("C-c h" . point-history-show)
  (:map point-history-show-mode-map
        ("q" . point-history-close))
  :config
  (setq point-history-save-timer 5)
  (setq point-history-show-buffer-height 20)
  (setq point-history-ignore-buffer "\\*.*\\*")
  (defun point-history-set-point ()
    (interactive)
    (point-history--update-list!))
  (advice-add 'consult-ripgrep :before 'point-history-set-point)
  (advice-add 'consult-line :before 'point-history-set-point)
  :init
  (point-history-mode t))

(use-package wgrep
  :ensure t
  :config
  (defun custom-wgrep-apply-save ()
    "Apply the edits and save the buffers"
    (interactive)
    (wgrep-finish-edit)
    (wgrep-save-all-buffers))

  (setq wgrep-change-readonly-file t)
  :bind (:map wgrep-mode-map
              ("C-x C-s" . custom-wgrep-apply-save)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         ("M-o" . other-window)
         ("C-x 0" . ace-delete-window)
         ("C-x O" . ace-swap-window)
         ("C-x M-0" . delete-other-windows)))

(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.35)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?\;
                      ?v ?b ?n ?. ?, ?/ ?u ?p ?e
                      ?c ?q ?2 ?3 ?'))
  (setq avy-dispatch-alist '((?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?x . avy-action-copy-whole-line)
                             (?X . avy-action-kill-whole-line)
                             (?t . avy-action-teleport)

                             (?m . avy-action-mark)
                             (?M . avy-action-mark-to-char)
                             (?w . avy-action-copy)
                             (?y . avy-action-yank)
                             (?Y . avy-action-yank-line)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?o . avy-action-embark)))
  :custom
  (avy-single-candidate-jump nil)
  :bind*
  ("C-j" . avy-goto-char-timer)
  ("M-m" . avy-goto-word-0))

;;;###autoload
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

;;;###autoload
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

;;;###autoload
(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

;;;###autoload
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

;;;###autoload
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

;;;###autoload
(defun avy-action-embark (pt)
  (save-excursion
    (goto-char pt)
    (embark-act))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rg
  :ensure t
  :defer 3)

(use-package fzf
  :ensure t
  :bind
  ("C-c C-x C-f" . fzf-find-file)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(provide 'my-navigation)
