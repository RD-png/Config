;;; defaults.el -*- lexical-binding: t; -*-

(setq auto-mode-case-fold nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;;; Misc.
(setq undo-limit 400000
      undo-strong-limit 3000000
      undo-outer-limit 48000000
      system-uses-terminfo nil
      backup-inhibited t
      auto-save-default nil
      initial-scratch-message ""
      require-final-newline t)
(global-subword-mode 1)


;;; Scrolling.
(setq fast-but-imprecise-scrolling t)


;;; User Interface.
(setq frame-inhibit-implied-resize t)
(setq idle-update-delay 1.0)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(setq inhibit-compacting-font-caches t)
(setq pgtk-wait-for-event-timeout 0.001)

(setq read-process-output-max (* 64 1024))


;;; Font.
(setq redisplay-skip-fontification-on-input t)


;;; GC.
(use-package gcmh
  :ensure t
  :defer 2
  :config
  (setq gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 32 1024 1024))
  (gcmh-mode 1))


;;; Files.
(setq find-file-visit-truename t
      vc-follow-symlinks t)
(setq find-file-suppress-same-file-warnings t)

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

(defvar doom-inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening files.")

(defvar doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)

(defvar doom-large-file-size-alist '(("." . 1.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.
If a file is opened and discovered to be larger than the threshold, Doom
performs emergency optimizations to prevent Emacs from hanging, crashing or
becoming unusably slow.
These thresholds are in MB, and is used by `doom--optimize-for-large-files-a'.")

(defvar doom-large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
                 git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
                 ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom-check-large-file-h' will ignore.")

(defadvice! doom--prepare-for-large-files-a (size _ filename &rest _)
  "Sets `doom-large-file-p' if the file is considered large.
Uses `doom-large-file-size-alist' to determine when a file is too large. When
`doom-large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  :before #'abort-if-file-too-large
  (and (numberp size)
       (null doom-inhibit-large-file-detection)
       (ignore-errors
         (> size
            (* 1024 1024
               (assoc-default filename doom-large-file-size-alist
                              #'string-match-p))))
       (setq-local doom-large-file-p size)))

(add-hook 'find-file-hook
          (defun doom-optimize-for-large-files-h ()
            "Trigger `so-long-minor-mode' if the file is large."
            (when (and doom-large-file-p buffer-file-name)
              (if (or doom-inhibit-large-file-detection
                      (memq major-mode doom-large-file-excluded-modes))
                  (kill-local-variable 'doom-large-file-p)
                (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
                  (so-long-minor-mode +1))
                (message "Large file detected! Cutting a few corners to improve performance...")))))


;;; Formatting.
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq tabify-regexp "^\t* [ \t]+")
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(add-hook 'text-mode-hook #'visual-line-mode)


;;; Kill Ring.
(setq kill-do-not-save-duplicates t)


;;; Built-in.
(use-package dired
  :ensure nil
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . toggle-truncate-lines))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("q" . dired-up-directory))
  :custom
  ((dired-listing-switches "-AGFhlv --group-directories-first")
   (dired-recursive-copies t))
  :config
  ;; (setf dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-hide-details-hide-symlink-targets nil))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :commands recentf-open-files
  :custom (recentf-save-file (concat user-emacs-directory "var/recentf"))
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)

  (defun my/recentf-file-truename (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
      file))
  (add-to-list 'recentf-filename-handlers #'my/recentf-file-truename)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :ensure nil
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "var/savehist.el"))
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables
        '(kill-ring
          register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(use-package server
  :ensure nil
  :when (display-graphic-p)
  :custom (server-auth-dir (concat user-emacs-directory "server/"))
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :config
  (unless (server-running-p)
    (server-start)))

(use-package tramp
  :ensure nil
  :defer 2
  :custom
  (tramp-default-method "ssh")
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-completion-reread-directory-timeout 60
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))

(use-package helpful
  :ensure t
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (setq apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(use-package info-colors
  :ensure t
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package smartparens
  :ensure t
  :bind
  ("C-M-w" . sp-copy-sexp)
  ("C-M-k" . sp-kill-sexp)
  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-show-pair-from-inside t)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-navigate-consider-symbols nil)
  (sp-local-pair '(emacs-lisp-mode scheme-mode clojure-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode scheme-mode clojure-mode) "`" "`" :actions nil)
  :init
  (smartparens-global-mode +1))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (advice-add 'lsp :before (lambda (&optional n) (direnv-update-environment)))
  (direnv-mode))

(use-package no-littering
  :ensure t)

(provide 'core)
