;;; my-theme.el -*- lexical-binding: t; -*-

;;; Font.
(defvar default-font-size 120)
(defvar default-variable-font-size 110)
(set-face-attribute 'default nil
                    :font "Fantasque Sans Mono"
                    :foundry "PfEd"
                    :slant 'normal
                    :weight 'normal
                    :width 'normal
                    :height default-font-size)

(set-face-attribute 'fixed-pitch nil
                    :font "Fantasque Sans Mono"
                    :height default-font-size)

(set-face-attribute 'variable-pitch nil
                    :font "Fantasque Sans Mono"
                    :height default-variable-font-size
                    :weight 'regular)


;;; Theme.
(setq custom-safe-themes t)
(setq custom--inhibit-theme-enable nil)
(setq x-underline-at-descent-line t)
(set-face-attribute 'cursor nil :background "IndianRed3")

(use-package modus-themes
  :ensure (modus-themes :host github :repo "protesilaos/modus-themes")
  :disabled t
  :config
  (setq modus-themes-common-palette-overrides
        `((fringe unspecified)
          (underline-paren-match fg-main)
          (bg-region bg-ochre)
          (fg-region unspecified)
          (bg-paren-match bg-blue-intense)
          (bg-mode-line-active bg-lavender)
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (bg-line-number-inactive unspecified)
          (bg-prompt bg-blue-nuanced)
          (fg-prompt blue-warmer)
          (fg-heading-1 blue-faint)
          (fg-heading-2 "DarkGoldenrod2")
          (fg-heading-3 "PaleVioletRed1")
          (cursor "IndianRed3")))
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-deuteranopia)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(bold background)
        modus-themes-headings '((1 . (variable-pitch light 1.25))
                                (2 . (variable-pitch light 1.20))
                                (t . (regular 1.15)))
        modus-themes-region '(no-extend accented bg-only)

        modus-operandi-palette-overrides `((bg-main "#f4f0ec")
                                           (fg-line-number-active "#0000b0")
                                           (comment red-faint)
                                           (string olive)
                                           ;; ,@modus-themes-preset-overrides-warmer
                                           )

        modus-vivendi-deuteranopia-palette-overrides '((bg-main "#1d2021")
                                                       (fg-main "#dfdfdf")
                                                       (string olive)
                                                       (comment red-faint)
                                                       (magenta-cooler magenta-faint)
                                                       (magenta-warmer pink)
                                                       (cyan slate)
                                                       (blue-cooler blue-warmer)
                                                       (fg-line-number-active "#79a8ff")))
  (load-theme 'modus-vivendi-deuteranopia t))

(use-package ef-themes
  :ensure t
  :disabled t
  :config
  (set-face-attribute 'bold nil :weight 'regular)
   (setq ef-themes-common-palette-overrides
         '((bg-main "#1d2021")
           (comment red-faint)))
  (load-theme 'ef-dream))


(use-package circadian
  :ensure t
  :disabled t
  :config
  (setq circadian-themes '(("8:00" . modus-operandi)
                           ("18:00" . modus-vivendi-deuteranopia)))
  (circadian-setup))

(use-package standard-themes
  :ensure (standard-themes :host github :repo "protesilaos/standard-themes")
  :preface
  (setq pdf-view-midnight-colors '("#dfdfdf" . "#1d2021"))
  (setq standard-themes-common-palette-overrides '((cursor "IndianRed3")))
  (setq standard-light-palette-overrides '((bg-main "#fff5ee")
                                           (fringe "#fff5ee")
                                           (fg-line-number-active "#0000b0")
                                           (bg-mode-line-active "#d0d6ff")
                                           (string "#316500")))
  (setq standard-dark-palette-overrides '((bg-main "#1d2021")
                                          (fg-main "#dfdfdf")
                                          (fringe "#1d2021")
                                          (fg-line-number-active "#79a8ff")
                                          (bg-mode-line-active "#484d67")
                                          (comment "#ff8f88")
                                          (string "DarkOliveGreen3")
                                          (cyan magenta-faint)))
  :init
  (load-theme 'standard-light t))

(use-package ligature
  :ensure (ligature :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  :config
  (global-ligature-mode +1))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 10)
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp nil)

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages buffer-info)
    '(misc-info matches major-mode process vcs))
  :init
  (doom-modeline-mode +1))

(use-package nano-modeline
  :ensure t
  :disabled t
  :config
  (setq nano-modeline-prefix 'status)
  (setq nano-modeline-prefix-padding 1)

  (defun my/thin-modeline ()
  "Transform the modeline in a thin faded line"

  (nano-modeline-face-clear 'mode-line)
  (nano-modeline-face-clear 'mode-line-inactive)
  (setq mode-line-format (list ""))
  (setq-default mode-line-format (list ""))
  (set-face-attribute 'mode-line nil
                      :inherit 'nano-modeline-inactive
                      :height 0.1)
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'nano-modeline-inactive
                      :height 0.1))

  (add-hook 'nano-modeline-mode-hook #'my/thin-modeline)

  (set-face-attribute 'header-line nil)
  (set-face-attribute 'nano-modeline-active-name nil
                      :foreground "black"
                      :inherit '(nano-modeline-active nano-strong))
  (set-face-attribute 'nano-modeline-active-primary nil
                      :inherit '(nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-secondary nil
                      :inherit '(nano-faded nano-modeline-active))

  (set-face-attribute 'nano-modeline-active-status-RW nil
                      :inherit '(nano-faded-i nano-strong nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-status-** nil
                      :inherit '(nano-popout-i nano-strong nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-status-RO nil
                      :inherit '(nano-default-i nano-strong nano-modeline-active))

  (set-face-attribute 'nano-modeline-inactive-name nil
                      :inherit '(nano-faded nano-strong
                                            nano-modeline-inactive))
  (set-face-attribute 'nano-modeline-inactive-primary nil
                      :inherit '(nano-faded nano-modeline-inactive))

  (set-face-attribute 'nano-modeline-inactive-secondary nil
                      :inherit '(nano-faded nano-modeline-inactive))
  (set-face-attribute 'nano-modeline-inactive-status-RW nil
                      :inherit '(nano-modeline-inactive-secondary))
  (set-face-attribute 'nano-modeline-inactive-status-** nil
                      :inherit '(nano-modeline-inactive-secondary))
  (set-face-attribute 'nano-modeline-inactive-status-RO nil
                      :inherit '(nano-modeline-inactive-secondary))
  :init
  (nano-modeline-mode 1))

(use-package smart-mode-line
  :ensure t
  :disabled t
  :commands sml/setup
  :init
  (setq sml/theme nil)
  (sml/setup)
  :config
  (setq sml/show-file-name nil)
  (setq mode-line-default-help-echo nil
        show-help-function nil)
  (add-to-list 'sml/replacer-regexp-list '("^~/[dD]ocuments/[rR]oam.*/" ":ROAM:")))

(use-package minions
  :ensure t
  :custom
  (minions-prominent-modes '(flymake-mode))
  :init
  (minions-mode +1))

(provide 'my-theme)
