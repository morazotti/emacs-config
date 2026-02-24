(setq global-hl-line-mode t
      visual-bell t
      show-paren-mode t)
(set-fringe-mode 10)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defvar my/fixed-font "Ligamonacop")
(defvar my/variable-font "CMU Serif")
(defvar my/fixed-font-height 120)
(defvar my/variable-font-height 140)

(defun my/setup-fonts (&optional frame)
  "Configure default and variable-pitch faces for the given FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-face-attribute 'default nil
                          :font my/fixed-font
                          :height my/fixed-font-height)
      (set-face-attribute 'variable-pitch nil
                          :font my/variable-font
                          :height my/variable-font-height)
      (setq mixed-pitch-set-height t))))

(add-hook 'after-make-frame-functions #'my/setup-fonts)
(my/setup-fonts)

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font my/fixed-font
                      :height 120))

(when (display-graphic-p)
  (set-face-attribute 'variable-pitch nil
                      :font my/variable-font
                      :height 140))

;; old
;; (setq default-font "Ligamonacop 12")
;; (set-frame-font default-font nil t)
;; (setq default-frame-alist '((font . "Ligamonacop 12")))

;; all-the-icons
;; (use-package all-the-icons
;;   :ensure (:host github :repo "domtronn/all-the-icons.el" :branch "master"))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; theme
(defvar my/theme 'base16-catppuccin-latte) ;;anterior: 'doom-tokyo-night
(use-package base16-theme
  :demand t
  :config (load-theme my/theme t nil))

;; visual-shorthands-mode - hide long prefixes made easy
(use-package visual-shorthands-mode
  :ensure (:host github
	   :repo "morazotti/visual-shorthands.el"
	   :branch "feat/region-as-default-longhand"
	   :main "visual-shorthands.el")
  :config (global-visual-shorthands-mode 1))

;; ligature
(use-package ligature
  :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'org-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                      "..." "+++" "/==" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "__" "~~" "(*" "*)"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "__" "~~" "(*" "*)")))
  (global-ligature-mode))

;; doom modeline
(use-package doom-modeline
  :demand t
  :ensure (:host github :repo "seagle0128/doom-modeline" :branch "master")
  :custom ((doom-modeline-buffer-file-name-style 'buffer-name)
	   (doom-modeline-buffer-encoding nil))
  :config
  (doom-modeline-mode)
  (add-hook 'after-make-frame-functions (lambda (frame)
                                                  (setq doom-modeline-icon t))))

(provide 'core-ui-config)
