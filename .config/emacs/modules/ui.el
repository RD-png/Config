;;; ui.el -*- lexical-binding: t; -*-

(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

(setq mouse-yank-at-point t)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 8
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;; Cursor.
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)


;;; Fringes.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;; Line Numbers.
;; (setq-default display-line-numbers-width 1)
;; (setq-default display-line-numbers-widen t)


;;; windows.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


;;; Minibuffers.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(advice-add #'yes-or-no-p :override #'y-or-n-p)


;;; Misc.
(when (window-system) (setq confirm-kill-emacs 'yes-or-no-p))
(advice-add 'display-startup-echo-area-message :override #'ignore)


;;; Built-in.
(setq ansi-color-for-comint-mode t)

(use-package display-fill-column-indicator
  :ensure nil
  :config
  (setq-default fill-column 78)
  :custom-face
  (fill-column-indicator ((t (:background "light blue")))))

(use-package comint
  :ensure nil
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

(use-package compile
  :ensure nil
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package window
  :ensure nil
  :config
  (setq display-buffer-alist
        `(("\\*\\(scheme\\|eshell\\|vterm\\)\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0))
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))))
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq window-sides-vertical nil)
  (setq window-combination-resize t))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :preface (defvar winner-dont-bind-my-keys t)
  :bind
  ("C-x /" . winner-undo)
  ("C-x ?" . winner-redo)
  :config
  (setq winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
                                "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
                                "*esh command on file*")))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-ancestor-buffer t))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  :init
  (show-paren-mode 1))

(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
             trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))


;;; External.
(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(setq image-animate-loop t)

(use-package dashboard
  :ensure t
  :bind
  ("C-x c e" . dashboard-refresh-buffer)
  :config
  (setq dashboard-startup-banner 'official)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-set-file-icons t)
  ;; :init
  ;; (dashboard-setup-startup-hook)
  )

(use-package popper
  :ensure t
  :bind (("C-x C-." . popper-toggle-latest)
         ("C-x M-." . popper-kill-latest-popup)
         ("C-x C-/" . popper-cycle)
         ("C-x C-;" . popper-toggle-type))
  :init
  (setq popper-window-height 15
        popper-mode-line nil)
  (setq even-window-sizes nil)
  (setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))
  (setq popper-reference-buffers
        (append
         '("\\*Messages\\*"
           "\\*scheme\\*"
           "\\*erlang\\*"
           "\\*ielm\\*"
           "\\*eshell\\*"
           "\\*vterm\\*"
           "^\\*Warnings\\*$"
           "Output\\*$"
           "^\\*Backtrace\\*"
           "\\*Async Shell Command\\*"
           "\\*Completions\\*"
           "\\*devdocs\\*"
           "[Oo]utput\\*"
           "*eldoc*"
           "*eldoc *.*$"
           "*helpful command: *.*$"
           "*helpful function: *.*$"
           "*helpful variable: *.*$"
           help-mode
           compilation-mode)))

  (defun popper-message-shorten (name)
    (cond
     ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(H)"))
     ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(H)"))
     ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              (if (string-empty-p (match-string 1 name)) "shell(E)" "(E)")))
     ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
      (concat (match-string 1 name)
              "(O)"))
     ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
      (concat (match-string 1 name)
              "(L)"))
     ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
      (concat (match-string 1 name)
              "(C)"))
     (t name)))

  (setq popper-echo-transform-function #'popper-message-shorten)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 1))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package diredfl
  :ensure t
  :disabled t
  :hook (dired-mode . diredfl-mode)
  :init
  (setq diredfl-ignore-compressed-flag nil)
  (diredfl-global-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (Info-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width 75))

(use-package hide-mode-line
  :ensure t
  :hook (vterm-mode . hide-mode-line-mode)
  :hook (Info-mode . hide-mode-line-mode))

(provide 'ui)
