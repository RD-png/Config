;;; my-editing.el -*- lexical-binding: t; -*-

(use-package embrace
  :ensure t
  :bind
  ("C-c c a" . embrace-add)
  ("C-c c c" . embrace-change)
  ("C-c c d" . embrace-delete))

(use-package isearch
  :ensure nil
  :bind
  ("M-s" . isearch-forward-thing-at-point)
  ("M-r" . isearch-forward-thing-at-point)
  :config
  (define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward))

(use-package expand-region
  :ensure t
  :bind (("C-}" . er/expand-region)
         ("C-M-}" . er/mark-outside-pairs)
         ("C-{" . er/select-text-in-delims)))

(use-package anzu
  :ensure t
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :config
  (global-anzu-mode +1))

(use-package paredit
  :ensure t
  :config
  (defun my/paredit-forward-down ()
    (interactive)
    (sp-down-sexp)
    (sp-next-sexp))

  (defun my/paredit-start-of-sexp ()
    (interactive)
    (when (not (= ?\( (following-char)))
      (progn
        (sp-end-of-sexp)
        (sp-forward-sexp)))
    (backward-sexp))

  (defun my/paredit-end-of-sexp ()
    (interactive)
    (when (not (= ?\( (following-char)))
      (progn
       (sp-beginning-of-sexp)
       (sp-backward-sexp)))
    (forward-sexp)
    (backward-char))

  (setcdr paredit-mode-map nil)
  :bind ((:map paredit-mode-map
               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . paredit-backward)
               ("C-M-d" . sp-down-sexp)
               ("C-M-u" . sp-up-sexp)
               ("C-M-f" . my/paredit-forward-down)
               ("C-M-b" . paredit-backward-up)
               ("C-M-a" . my/paredit-start-of-sexp)
               ("C-M-e" . my/paredit-end-of-sexp)
               ("M-[" . paredit-backward-slurp-sexp)
               ("M-]" . paredit-forward-slurp-sexp)
               ("M-{" . paredit-backward-barf-sexp)
               ("M-}" . paredit-forward-barf-sexp)
               ("C-M-r" . paredit-raise-sexp)
               ("M-j" . paredit-join-sexp)
               ("C-M-o" . paredit-splice-sexp)
               ("C-k" . paredit-kill))))

(use-package evil-nerd-commenter
  :ensure t
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-mode 1)
  (setq rainbow-delimiters-max-face-count 4))

;; Colors for # colors
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         typescript-mode
         web-mode))

(use-package yasnippet
  :ensure t
  :demand t
  :custom
  (yas-triggers-in-field t)
  :init
  (yas-global-mode 1)
  ;; :config
  ;; (defun do-yas-expand ()
  ;;   (let ((yas/fallback-behavior 'return-nil))
  ;;     (yas/expand)))
  ;;
  ;; (defun tab-complete-or-next-field ()
  ;;   (interactive)
  ;;   (if (or (not yas/minor-mode)
  ;;           (null (do-yas-expand)))
  ;;       (if corfu--candidates
  ;;           (progn
  ;;             (corfu-insert)
  ;;             (yas-next-field))
  ;;         (yas-next-field))))
  ;; :bind (:map yas-keymap
  ;;             ("<tab>" . tab-complete-or-next-field))
  )

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :init
  (setq-default flymake-diagnostic-functions nil)
  (with-eval-after-load 'flymake-proc
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
  :config
  (setq flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t))

(use-package flymake-diagnostic-at-point
  :ensure t
  :disabled t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-timer-delay 0.66))

(use-package flymake-popon
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method 'posframe
        flymake-popon-delay 0.66))

(use-package flyspell
  :ensure nil
  :hook (text-mode . flyspell-mode))

;;;###autoload
(defun er/select-text-in-delims ()
  (interactive)
  (let ( $skipChars $p1 )
    (setq $skipChars "^\"'`<>(){}[]‹›«»")
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this))
  :config
  (setq mc/always-repeat-command t))

(defun my/beginning-of-line ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
	  (beginning-of-visual-line)))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

;;;###autoload
(defun shift-right (count)
  (interactive "p")
  (shift-text count))

;;;###autoload
(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

;;;###autoload
(defun my/backward-kill-word ()
  "Smarter C-Backspace control"
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (string-blank-p (string-trim substr)))
                        (string-match-p "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))))

(defun my/op-thing-at-point (op thing)
  "Get the start and end bounds of a type of thing at point."
  (superword-mode 1)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (funcall op (car bounds) (cdr bounds))
      (error "No %s at point" thing)))
  (superword-mode -1)
  (global-subword-mode 1))

;;;###autoload
(defun multi-line-next ()
  (interactive)
  (forward-line 5))

;;;###autoload
(defun multi-line-prev ()
  (interactive)
  (forward-line -5))

;;;###autoload
(defun my/scroll-up ()
  (interactive)
  (scroll-up-command)
  (recenter))

;;;###autoload
(defun my/scroll-down ()
  (interactive)
  (scroll-down-command)
  (recenter))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; General binds
(global-set-key (kbd "C-x f") #'find-file)
(global-set-key (kbd "C-x c f") (lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-x c r") (lambda () (interactive) (find-file "~/.config/emacs/org/Roam/")))
(global-set-key (kbd "C-c w") (lambda () (interactive) (my/op-thing-at-point 'copy-region-as-kill 'word)))
(global-set-key (kbd "M-d") (lambda () (interactive) (my/op-thing-at-point 'delete-region 'word)))

(global-set-key (kbd "M-]") #'shift-right)
(global-set-key (kbd "M-[") #'shift-left)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)
(global-set-key (kbd "C-M-<return>") #'vterm)
(global-set-key (kbd "C-S-k") #'kill-whole-line)
(global-set-key (kbd "C-x c e")  #'dashboard-refresh-buffer)
(global-set-key (kbd "C-c o g")  #'xref-find-definitions)
(global-set-key (kbd "C-/")  #'undo-only)
(global-set-key (kbd "C-?")  #'undo-redo)
(global-set-key (kbd "C-S-n")  #'multi-line-next)
(global-set-key (kbd "C-S-p")  #'multi-line-prev)
(global-set-key (kbd "C-M-S-n")  #'move-line-down)
(global-set-key (kbd "C-M-S-p")  #'move-line-up)
(global-set-key [remap org-cycle-agenda-files] 'ignore)
(global-set-key (kbd "C-v") #'my/scroll-up)
(global-set-key (kbd "M-v") #'my/scroll-down)
(bind-key* "C-<backspace>" #'my/backward-kill-word)


;; unbind annoying keybinds
(global-unset-key  (kbd "C-x C-n"))
(global-unset-key  (kbd "M-`"))
(global-unset-key  (kbd "C-z"))
(global-unset-key  (kbd "C-x C-z"))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (back-to-indentation) (point)) (line-end-position)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Line Copied")
     (list (save-excursion (back-to-indentation) (point)) (line-end-position)))))

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defun pils-follow (&rest _arg)
  "Advice to follow a function which spawn a window."
  (other-window 1))

(advice-add 'split-window-below :after #'pils-follow)
(advice-add 'split-window-right :after #'pils-follow)

(provide 'my-editing)
