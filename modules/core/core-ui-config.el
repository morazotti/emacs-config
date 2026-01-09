(setq global-hl-line-mode t
      visual-bell t
      show-paren-mode t)
(set-fringe-mode 10)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defvar my/fixed-font "Ligamonacop")
(defvar my/variable-font "CMU Serif")

(defun my/set-fixed-font (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-face-attribute 'default nil
                          :font my/fixed-font
                          :height 120))))

(defun my/set-variable-font (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (progn
	(set-face-attribute 'variable-pitch nil
                          :font my/variable-font
                          :height 140)
	(setq mixed-pitch-set-height t)))))

(add-hook 'after-make-frame-functions #'my/set-fixed-font)
(add-hook 'after-make-frame-functions #'my/set-variable-font)

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
;;   :straight (:host github :repo "domtronn/all-the-icons.el" :branch "master"))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; theme
(use-package base16-theme)
;; (use-package doom-themes)
;; (use-package modus-themes
;;   :straight (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(defvar my/theme 'base16-catppuccin-latte) ;;anterior: 'doom-tokyo-night
(load-theme my/theme t nil)

;; ligature
(use-package ligature
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
(global-ligature-mode)

;; doom modeline
(use-package doom-modeline
  :straight (:host github :repo "seagle0128/doom-modeline" :branch "master")
  :custom ((doom-modeline-buffer-file-name-style 'buffer-name)
	   (doom-modeline-buffer-encoding nil))
  :config (add-hook 'after-make-frame-functions (lambda (frame)
                                                  (setq doom-modeline-icon t))))
(doom-modeline-mode)

(provide 'core-ui-config)
