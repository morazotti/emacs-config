	```{=org}
#+STARTUP: overview
```
This is a modular Emacs configuration designed for performance,
scientific writing, and a hybrid Vim/Emacs workflow. The setup is
divided into functional modules to maintain clarity and ease of
maintenance.

# Initialization

The startup process begins with the early-init file to optimize the UI
and memory management, followed by the main init file which orchestrates
the loading of all sub-modules.

## init-async (silent background org export)

This config includes `init-async.el`, a small helper that can run some
tasks in the background after startup. In particular, it can export Org
files **silently** (without popping buffers/windows) so you can, for
example, regenerate documentation like `README.md` from `README.org`
without interrupting your session.

-   Config / entry point: `init-async.el`

``` elisp
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(dolist (module '("core" "org" "langs"))
  (add-to-list 'load-path
	       (expand-file-name
		(format "modules/%s" module)
		user-emacs-directory)))

(require 'core-packages-config)
(require 'core-variables-config)
(require 'org-core-config)
(require 'org-babel-config)
(require 'org-link-config)
(require 'org-ox-config)
(require 'langs-latex-config)
```

`early-init.el`
:   Configures the frame UI before it\'s created and maximizes garbage
    collection thresholds for a faster startup.

``` elisp
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
```

`init.el`
:   The main entry point. It sets up the load paths, initializes
    modules, and restores editor performance settings after startup.

``` elisp
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(let ((file-name-handler-alist-original file-name-handler-alist)
      ;; Ensure load-path/module loading is stable even if init.el is loaded from a
      ;; non-standard location.
      (default-directory user-emacs-directory))
  (setq file-name-handler-alist nil)

  (when (file-exists-p custom-file) (load custom-file))

  ;; Add each module dir (modules/<name>/) to `load-path`.
  (dolist (module '("core" "org" "tools" "langs" "ai"))
    (add-to-list 'load-path
                 (expand-file-name (format "modules/%s" module)
                                   user-emacs-directory)))

  ;; if we ever need to M-x straight-freeze-versions, we need to uncomment the following line before
  ;; (load (expand-file-name "modules/core/core-packages-config.el" user-emacs-directory))

  (require 'core-config)
  (require 'org-config)
  (require 'tools-config)
  (require 'langs-config)
  (require 'ai-config)

  ;; (require 'wip-functions)

  (setq file-name-handler-alist file-name-handler-alist-original))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
```

`emacs-custom.el`
:   Stores variables and faces generated through the Emacs Customization
    UI, kept separate to prevent cluttering the main configuration.

``` elisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(ebib-reading-list-template "* %M %T\12\12\12:PROPERTIES:\12%K\12:END:\12")
 '(org-hide-emphasis-markers t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-safe-remote-resources '("\\`\\[\\[file:early-init\\.el]]\\'"))
 '(org-use-sub-superscripts nil)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (shell-command "pandoc README.org -o README.md"))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(jinx-misspelled ((t (:underline (:color "red" :style wave :position nil)))))
 '(org-document-title ((t (:foreground "#fe640b" :weight bold))))
 '(org-drawer ((t (:foreground "Blue1" :family "Ligamonacop"))))
 '(visual-shorthands-face ((t (:inherit font-lock-keyword-face :extend nil :background "#e0e0e0")))))
```

# Core Configuration

These modules define the fundamental editor experience, from package
management to the visual theme and keybinding philosophy.

`core-packages-config.el`
:   Bootstraps `straight.el` as the package manager and sets up
    `use-package` for declarative configuration.

``` elisp
;; packages.el
;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package project
  :straight t
  :ensure t)

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'core-packages-config)
```

`core-variables-config.el`
:   Centralizes global settings, directory paths for backups/undo
    history, and environment variables.

``` elisp
(defconst home (expand-file-name "~"))
(setq-default abbrev-mode t)
(setq my/library-directory (file-name-concat home "Dropbox" "my_library/")
      my/bibliography-file (file-name-concat my/library-directory "referencias.bib")
      my/pdf-library (file-name-concat my/library-directory  "pdfs")
      warning-minimum-level :emergency
      org-roam-directory (file-name-concat home "Documents" "roam")
      ispell-dictionary "pt_BR"
      fill-column 72

      ;; authinfo
      auth-sources '("~/.authinfo.gpg" "~/.authinfo")

      make-backup-files t
      backup-by-copying t              ; evita problemas com links/permissões
      version-control t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 10
      backup-directory-alist `(("." . ,(expand-file-name "~/.cache/emacs/backups/")))
      backup-inhibited nil

      ;; autosaves
      auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.cache/emacs/autosaves/") t))
      auto-save-default t

      ;; undo history
      undo-limit        800000
      undo-strong-limit 12000000
      undo-auto-save-history t
      undo-directory-alist `(("." . "~/.cache/emacs/undo/")))

(setenv "SSH_AUTH_SOCK" (format "%s/ssh-agent.socket" (getenv "XDG_RUNTIME_DIR")))
(put 'narrow-to-region 'disabled nil)
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(provide 'core-variables-config)
```

`core-ui-config.el`
:   Configures the visual interface, including the theme, fonts for both
    fixed and variable pitch, ligatures, and the modeline.

``` elisp
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

;; visual-shorthands-mode - hide long prefixes made easy
(use-package visual-shorthands-mode
  :straight (:host github
	     :repo "morazotti/visual-shorthands.el"
	     :branch "feat/region-as-default-longhand")
  :config (global-visual-shorthands-mode 1))

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
```

`core-keybindings-config.el`
:   Implements `evil-mode` for Vim emulation and defines a central
    leader-key system using `general.el`.

``` elisp
(defun my/duplicate-line ()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (insert "\n" line))))

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)
(global-set-key (kbd "C-,") 'my/duplicate-line)

;; vim-keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)

  :config
  (define-key evil-visual-state-map (kbd "=") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "-") 'er/contract-region)

  :hook
  (delve-mode . turn-off-evil-mode))
(evil-mode)

(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'delve-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)
(evil-set-initial-state 'ebib-log-mode 'emacs)
(evil-set-initial-state 'ebib-index-mode 'emacs)
(evil-set-initial-state 'ebib-entry-mode 'emacs)
(evil-set-initial-state 'ebib-strings-mode 'emacs)
(evil-set-initial-state 'ebib-multiline-mode 'emacs)

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-mc
  :demand t
  :after evil
  :init (global-evil-mc-mode 1))

;; leader-key
(use-package general
    :config (general-evil-setup t)
    (general-create-definer my/leader-keys
      :keymaps '(normal visual)
      :prefix "SPC"))

(my/leader-keys
  ;; file and buffer general usage
  "SPC" 'find-file 
  "RET" 'consult-bookmark
  "," 'consult-buffer
  "be" 'eval-buffer
  "bb" 'ibuffer
  "bk" 'kill-buffer
  "bs" 'save-buffer
  "." 'dired
  "z" 'zoom-window-zoom

  ;; commands
  "x" 'execute-extended-command
  ":" 'eval-expression
  ";" 'avy-goto-line
  "j" 'my/duplicate-line

  ;; movement
  "TAB" 'other-window
  "s" 'avy-goto-char

  ;; jinx
  "tb" 'jinx-correct-all
  "tw" 'jinx-correct-nearest

  ;; windows
  "wk" 'delete-window
  "ww" 'delete-other-windows
  "wh" 'my/split-and-follow-horizontally
  "wv" 'my/split-and-follow-vertically
  "wp" 'winner-undo
  "wn" 'winner-redo
  "w-" 'balance-windows
  ;; "wt" 'transpose-frame
  "wo" 'ace-window

  ;; open - general
  "om" 'notmuch
  "of" 'find-file-other-window
  "o," 'consult-buffer-other-window

  ;; project.el
  "pf" 'project-find-file
  "pb" 'consult-project-buffer
  "pc" 'project-compile
  "pp" 'project-switch-project
  "pk" 'project-kill-buffers
  "ps" 'project-shell
  "p!" 'project-shell-command

  ;; help
  "ha" 'apropos-command
  "hf" 'describe-function
  "hk" 'describe-key
  "hv" 'describe-variable
  "hd" 'eldoc
  "h." 'eldoc-box-help-at-point

  ;; register
  "rs" 'consult-register-store
  "rl" 'consult-register-load
  "rr" 'consult-register

  ;; org roam
  "rf" 'org-roam-node-find
  "rc" 'org-roam-capture
  "rb" 'org-roam-buffer-toggle
  "rI" 'org-roam-node-insert-immediate
  "ri" 'org-roam-node-insert
  "ra" 'org-roam-alias-add
  "r]" 'org-roam-ref-find
  ;; "rd" 'org-roam-dailies-goto-today
  ;; "rg" 'org-roam-dailies-find-date
  "rt" 'org-transclusion-add

  ;; consult
  "/"  'consult-ripgrep
  "cd" 'consult-find
  "cG" 'consult-git-grep

  ;; org ref
  "[" 'citar-open
  "]" 'org-cite-insert
  ")" 'consult-reftex-insert-reference

  ;; snippets
  "yi" 'yas-insert-snippet
  "yn" 'yas-new-snippet
  "yv" 'yas-visit-snippet-file

  ;; agenda
  "aa" 'org-agenda
  "a[" 'org-agenda-file-to-front
  "a]" 'org-remove-file

  ;; store link
  "ls" 'org-store-link
  "li" 'org-insert-link

  ;; smart-parens
  "(k" 'sp-unwrap-sexp
  "((" 'sp-rewrap-sexp

  ;; ;; multiple cursors
  ;; "@" 'evil-multiedit-toggle-marker-here
  ;; "m@" 'evil-multiedit-match-all
  ;; "mn" 'evil-multiedit-match-and-next
  ;; "mp" 'evil-multiedit-match-and-prev

  ;; narrow
  "ns" 'org-narrow-to-subtree
  "nn" 'narrow-to-region
  "np" 'narrow-to-page
  "nd" 'narrow-to-defun
  "nw" 'widen

  ;; vundo
  "u" 'vundo

  ;; elfeed
  "ee" 'elfeed

  ;; ebib
  "eb" 'ebib

  ;; terminal
  "tt" 'vterm
  "et" 'eshell

  ;; dape
  "dd" 'dape
  "di" 'dape-info
  "dk" 'dape-kill
  "dn" 'dape-next
  "dr" 'dape-restart
  "db" 'dape-breakpoint-toggle
  "dl" 'dape-breakpoint-log
  "dh" 'dape-breakpoint-hits
  "de" 'dape-breakpoint-expression
  "dB" 'dape-breakpoint-remove-all
  "dq" 'dape-quit
  "dR" 'dape-repl
  "dc" 'dape-continue
  "dw" 'dape-watch-dwim
  "dx" 'dape-evaluate-expression
  
  ;; other commands
  "ie" 'emoji-search
  "ip" 'org-paste-image
  "ev" 'org-babel-tangle
  "ex" 'org-export-dispatch)

(provide 'core-keybindings-config)
```

`core-utils-config.el`
:   Manages the completion stack (Vertico, Marginalia, Orderless) and
    essential navigation tools like Consult and Embark.

``` elisp
;; Ibuffer
(use-package ibuffer
    :bind  ("C-x C-b" . ibuffer)
    :config (setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("dired" (mode . dired-mode))
                   ("org" (mode . org-mode))
  		 
                   ("programming" (or
                                   (mode . sh-mode)
                                   (mode . c-mode)
                                   (mode . python-mode)
                                   (mode . c++-mode)))
                   ("latex" (mode . latex-mode))
                   ("document" (mode . pdf-view-mode))
                   ("image" (mode . image-mode)) 
                   ("magit" (name . "^\\Magit.*"))
                   ("IRC" (mode . erc-mode))
                   ("notmuch" (or
                               (name . "^.*otmuch.*$")
                               (mode . notmuch-hello-mode)
                               (mode . notmuch-search-mode)
                               (mode . notmuch-show-mode)
                               (mode . notmuch-tree-mode)
                               (mode . notmuch-message-mode)))
                   ("ledger" (or (mode . ledger-mode) (mode . ledger-report-mode)))
                   ("emms" (name . "^\\*EMMS.*"))
                   ("emacs" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")))
                   ))))
    :config (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-auto-mode 1)
                (ibuffer-switch-to-saved-filter-groups "default")))
    :config (setq ibuffer-show-empty-filter-groups nil)
    :config (setq ibuffer-expert t)
    :hook (ibuffer-mode . (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

;; ace-window - avy for windows
(use-package ace-window
  :custom (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  :bind (("C-x o" . other-window)
	 ("M-o" . ace-window)))

;; avy
(use-package avy
    :config (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :bind (("M-s M-s" . avy-goto-char)
           ("M-g M-g" . avy-goto-line)))

;; ivy - some things need it
;; (use-package ivy
;;   :demand t
;;   :config
;;   (ivy-mode 1)
;;   (ivy-mode -1))

;;consult - embark: better framework to find and act on things
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-c p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-make)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-reftex
  :after consult
  :straight (:host github :repo "karthink/consult-reftex" :branch "master"))

(use-package embark
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
    ;; strategy, if you want to see the documentation from multiple providers.
    (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    ;; jinx
    (keymap-set jinx-repeat-map "RET" 'jinx-correct)
    (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
    (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
    (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
    (add-to-list 'embark-repeat-actions #'jinx-next)
    (add-to-list 'embark-repeat-actions #'jinx-previous)
    (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target))
    (add-to-list 'embark-default-action-overrides (list jinx #'jinx-correct)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; vertico - marginalia - orderless: minibuffer framework
(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)

  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config (setq read-file-name-completion-ignore-case t
                read-buffer-completion-ignore-case t
                completion-ignore-case t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
    :custom
    (completion-styles '(orderless basic flex))
    (completion-category-overrides '((file (styles basic partial-completion)))))

;; wgrep - allows changing grep buffers
(use-package wgrep)
;;(require 'wgrep)

(provide 'core-utils-config)
```

`core-config.el`
:   Aggregates all core modules.

``` elisp
(require 'core-packages-config)
(require 'core-variables-config)
(require 'core-utils-config)
(require 'core-keybindings-config)
(require 'core-ui-config)

(provide 'core-config)
```

# Org Mode

Org-mode is the heart of this configuration, tailored for academic
research, note-taking, and scientific publishing.

`org-core-config.el`
:   Sets up the base Org-mode environment, utilizing a specific
    development branch for enhanced LaTeX previews.

``` elisp
;; autoload org package
(defun my/org-generate-version-file ()
  "Generate org-version.el for straight.el build process."
  (require 'lisp-mnt)
  (let ((version
         (with-temp-buffer
           (insert-file-contents "lisp/org.el")
           (lm-header "version")))
        (git-version
         (string-trim
          (with-temp-buffer
            (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
            (buffer-string)))))
    (with-temp-file "org-version.el"
      (insert
       (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
       (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
       "(provide 'org-version)\n"))))

(use-package org
  :straight (:host nil
             :repo "https://git.tecosaur.net/tec/org-mode.git"
             :branch "dev"
             :remote "tecosaur"
             :files (:defaults "etc")
             :build t
             :pre-build (my/org-generate-version-file))
  :demand t
  :init
  (setq org-directory (file-name-concat home "Documents" "org"))

  :custom ((org-src-fontify-natively t)
           (org-src-window-setup 'reorganize-frame)
           (org-confirm-babel-evaluate nil)

           ;; org-cite
           (org-cite-global-bibliography (list my/bibliography-file))
           (org-cite-insert-processor 'citar)
           (org-cite-follow-processor 'citar)
           (org-cite-activate-processor 'citar)

           ;; LaTeX
           (org-latex-listings 'minted)
           (org-latex-packages-alist '(("AUTO" "babel" t)
                                       ("" "physics" t)
                                       ("" "framed" t)
                                       ("" "tikz" t)))
           (org-latex-pdf-process '("latexmk -synctex=1 -shell-escape -bibtex -interaction=nonstopmode -pdf -f -8bit %f"))
           (org-latex-prefer-user-labels t)
           (org-format-latex-options
            '(:foreground default
                          :background default
                          :scale 2.0
                          :html-foreground "Black"
                          :html-background "Transparent"
                          :html-scale 1.0
                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[" )))

           (org-preview-latex-image-directory
            (file-name-concat home ".cache" "ltximg"))

           ;; ox
           (org-export-async-init-file (expand-file-name "init-async.el" user-emacs-directory)))

  :hook ((org-babel-after-execute . display-ansi-colors)
         (org-mode . org-indent-mode)
         (org-mode . org-latex-preview-mode)
         (org-mode . visual-line-mode)
         (org-mode . org-toggle-pretty-entities)
         (org-mode . (lambda ()
                       (add-hook
                        'after-save-hook
                        'my/org-export-run-on-save
                        nil 'make-it-local))))
  :bind (("C-c l" . org-store-link)
         (:map org-mode-map ("<f7>" . my/org-export-and-set-async-hook))))

;; some extra configs to org and org-export
(use-package org-contrib)
(use-package ox-extra
  :after (org org-contrib)
  :config (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(provide 'org-core-config)
```

`org-ui-config.el`
:   Improves Org buffer visuals (e.g., org-modern, mixed-pitch).

``` elisp
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package org-modern
  :after org
  :straight (:host github :repo "minad/org-modern" :branch "main")
  :custom ((org-modern-table nil)
	   (org-modern-label-border nil))
  :hook ((org-mode . org-modern-mode)
	 (org-mode . variable-pitch-mode)))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;; svg-tags: create buttons with svg
;; (use-package svg-lib)
;; (use-package svg-tag
;;   :ensure svg-lib
;;   :straight (:host github :repo "rougier/svg-tag-mode" :branch "main")
;;   :hook (org-mode . svg-tag-mode))
;; (setq svg-tag-tags
;;       '(("\\(:@[A-Za-z0-9]+\\)" . ((lambda (tag)
;;                                      (svg-tag-make (upcase tag) :beg 2))))
;;         ("\\(:@[A-Za-z0-9]+:\\)$" . ((lambda (tag)
;;                                        (svg-tag-make (upcase tag) :beg 2 :end -1))))
;;         ("\\(:@[A-Za-z]+:\\)" . ((lambda (tag) (svg-tag-make (upcase tag) :beg 2 :end -1))))
;;         ("\\(#.transclude: .*\\)" . ((lambda (tag) (svg-tag-make tag :beg 57 :end -2))
;;                                      (lambda () (interactive) (org-transclusion-add))))
;;         ("\\[#[A-Z]\\]" . ( (lambda (tag)
;;                               (svg-tag-make tag :face 'org-priority
;;                                             :beg 2 :end -1 :margin 1))))
;;         ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 1))))
;;         ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'org-todo :inverse t :margin 1))))
;;         ("BACKLOG" . ((lambda (tag) (svg-tag-make "BACKLOG" :face 'org-todo :inverse t :margin 1))))
;;         ("REVIEW" . ((lambda (tag) (svg-tag-make "REVIEW" :face 'org-todo :inverse t :margin 1))))
;;         ("IN-PROGRESS" . ((lambda (tag) (svg-tag-make "IN-PROGRESS" :face 'org-todo :inverse t :margin 1))))
;;         ("WAITING" . ((lambda (tag) (svg-tag-make "WAITING" :face 'org-todo :inverse t :margin 1))))
;;         ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 1))))
;;         ("ABANDONED" . ((lambda (tag) (svg-tag-make "ABANDONED" :face 'org-done :margin 1))))
;;         ("COMPLETE" . ((lambda (tag) (svg-tag-make "COMPLETE" :face 'org-done :margin 1))))
;;         ))

(provide 'org-ui-config)
```

`org-agenda-config.el`
:   Manages task lists and custom agenda views for project tracking and
    personal organization.

``` elisp
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/tasks.org")
                             (concat org-directory "/opusdei.org")))
(global-set-key (kbd "C-c a") 'org-agenda)

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current) subtree-end nil)))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""((org-agenda-skip-function
                       '(or (air-org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))))))
        ("p" "Planning" ((tags-todo "+@planning-@work")
                         (todo "BACKLOG" )))
        ("u" "Untagged Tasks" ((tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks")))
                               (todo ".*" ((org-agenda-files '("~/Documents/org/notes.org"))
                                           (org-agenda-overriding-header "Inbox"))))) 
        ("w" "Work" ((tags-todo "+@work" ((org-agenda-overriding-header "Work Tasks")))
                     (agenda "" ((org-agenda-overriding-header "Work Agenda")
                                 (org-agenda-files '("~/Documents/org/tasks.org"))
                                 (org-agenda-skip-entry-if 'notscheduled 'notdeadline)
                                 (org-agenda-span 'week)))))))

;; super agenda - not sure if I should use it again
;; (use-package org-super-agenda
;;   :init (org-super-agenda-mode 1)
;;   ;; :init (setq initial-buffer-choice (lambda () (get-buffer "*Org Agenda*")))
;;   :config (setq org-super-agenda-groups
;;                 '(
;;                   (:auto-property "ProjectId")

;;                   ;; (:name "blackgenn"
;;                   ;;        :scheduled today)
;;                   (:name "emacs" :tag "emacs")
;;                   (:name "books"
;;                          :and (:tag "book")))))

(provide 'org-agenda-config)
```

`org-roam-config.el`
:   Implements a Zettelkasten system for interconnected note-taking,
    featuring a visual graph and database sync.

``` elisp
(use-package org-roam
    :straight (:host github :repo "org-roam/org-roam" :branch "main")
    :config (org-roam-db-autosync-mode)
    :init (setq org-roam-v2-ack t)

    :custom ((org-roam-dailies-directory (file-name-concat org-roam-directory "projeto-pessoal"))
	     (org-roam-graph-link-hidden-types ("files" "https" "ref" "fuzzy")))

    :bind (("C-c r f" . org-roam-node-find)
           ("C-c r c" . org-roam-capture)
           ("C-c r b" . org-roam-buffer-toggle)
           ("C-c r I" . org-roam-node-insert-immediate)
           :map org-mode-map
           ("C-c r i" . org-roam-node-insert)))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  ((org-roam-ui-open-on-start nil)
   (org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t))
  :init
  ;; org-roam-ui-mode starts a websocket server; don't start it in batch.
  (unless noninteractive
    (org-roam-ui-mode))
  (setq org-roam-ui-latex-macros
        '(("\\Tr" . "\\mathrm{Tr}")
          ("\\tr" . "\\mathrm{Tr}")
          ("\\dyad" . "\\ket{#1}\\bra{#2}")
          ("\\order" . "\\mathcal{O}({#1})")
          ("\\I" . "\\mathbb{I}")
          ("\\norm" . "\\parallel{#1}\\parallel")
          ("\\id" . "\\mathbb{I}")
          ("\\expval" . "\\langle{#1}\\rangle")
          ("\\dd" . "\\mathrm{d}")
          ("\\op" . "|{#1}\\rangle\\langle{#2}|")
          ("\\label" . "\\vphantom")
          ("\\dv" . "\\frac{\\mathrm{d}{#1}}{\\mathrm{d}{#2}}")
          ("\\olra" . "\\overleftrightarrow{#1}"))))


(with-eval-after-load 'org-roam
  (add-to-list
   'org-roam-capture-templates
   '("r" "reference" plain
     "%?"
     :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${citar-title}\n#+roam_key: ${citar-citekey}\n")
     :node-properties (:ROAM_ALIASES "${citar-citekey}")
     :unnarrowed t)))

(provide 'org-roam-config)
```

`org-capture-config.el`
:   Defines templates for quickly capturing notes, tasks, and inbox
    items without breaking focus.

``` elisp
(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
         "* UNSEEN %?\n")
        ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
         "* TODO %?\n")
        ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
         "* TODO <%<%Y-%m-%d %H:%M:%S>> \n %?\n")
  	("t" "Tasks" entry (file+headline "~/Documents/org/tasks.org" "Tarefas")
         "* TODO %?\n")))
(global-set-key (kbd "C-c c") 'org-capture)
(provide 'org-capture-config)
```

`org-babel-config.el`
:   Configures literate programming environments, allowing code
    execution within Org files for various languages.

``` elisp
(require 'org-ob-jupyter-config)

(defvar my/org-latex-babel-lang-list
  '(C jupyter shell latex fortran python ledger))

(setq org-babel-load-languages '())
(dolist (lang my/org-latex-babel-lang-list)
  (add-to-list 'org-babel-load-languages (cons lang t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

(provide 'org-babel-config)
```

`org-ob-jupyter-config.el`
:   Adds Jupyter-backed Org Babel execution and helpers for connecting
    to existing kernels.

``` elisp
(use-package jupyter
  :straight
  (:host github
         :repo "emacs-jupyter/jupyter"
         :branch "master"
         :commit "f97f4b5d8c83e0b901020f835183dde8a2bf649e")
  :bind ("C-M-s-h" . jupyter-org-hydra/body))


;; manage ssh servers
;; we can set :session as
;; [/ssh:[USER@]HOST:]~/.local/share/jupyter/runtime/kernel-<PID>.json
;; if we name a session, otherwise, it launches a local ipykernel3
;; useful if we want to launch local uv venvs

(defun my/choose-ssh-host (ssh-config-file)
  "Read SSH config file and allow the user to choose a host.
    If the chosen host is not 'localhost', prefix it with '/ssh:<chosen-host>:'."
  (let ((hosts '("localhost"))
    	(kern-dir "~/.local/share/jupyter/runtime/"))  ;; Add the "local" option to the hosts list
    ;; Read the SSH config file
    (with-temp-buffer
      (insert-file-contents ssh-config-file)
      ;; Use regex to find "Host" definitions
      (while (re-search-forward "^Host\\s-+\\(.*\\)" nil t)
        (push (match-string 1) hosts)))
    ;; Use completing-read to select a host
    (let ((chosen-host (completing-read "Choose SSH host: " (reverse hosts))))
      (if (string= chosen-host "localhost")
          (message kern-dir)
        (message (concat "/ssh:" chosen-host ":" kern-dir))))))

(defun my/jupyter-runtime-folder ()
  (interactive)
  (expand-file-name (my/choose-ssh-host "~/.ssh/config"))) 

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (interactive)
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes (my/jupyter-runtime-folder) t ".*kernel")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: " files)))
;; (seq-filter
;;  (lambda (file)
;;    (member (cdr file) ports))
;;  files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel"
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                 (file-name-nondirectory (my/select-jupyter-kernel))))

(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete (seq-filter
		     (lambda (file)
		       (not (member (cdr file) ports)))
		     files)))
    (when (and (length> to-delete 0)
	       (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

(defun my/jupyter-launch-local-kernel ()
  (interactive)
  (let* ((venv (concat (getenv "VIRTUAL_ENV") ".venv/" ))
	 (process-environment process-environment)
	 (venv-string (concat "VIRTUAL_ENV=" venv))
	 (jupyter-bin (format "%sbin/jupyter" venv))
	 (cmd-kernelspec-list (format "%s %s kernelspec list" venv-string jupyter-bin))
	 (kernel-and-location
	  (completing-read "Choose kernel: "
			   (seq-remove #'string-empty-p (mapcar 'string-trim-left (cdr (split-string (shell-command-to-string cmd-kernelspec-list) "\n"))))
			   nil t))
	 (kernel
	  (when (string-match "\\([a-zA-Z0-9\-.]+\\) *\\(.*\\)" kernel-and-location)
	    (match-string 1 kernel-and-location)))
	 (command `(,jupyter-bin "kernel" "--kernel" ,kernel)))
    (make-process :name "virtual-jupyter"
		  :buffer "*Jupyter Kernel*"
		  :command command)))

(provide 'org-ob-jupyter-config)
```

`org-latex-config.el`
:   Provides specialized tools for mathematical writing, including
    `cdlatex` for fast snippet expansion.

``` elisp
(use-package cdlatex
      :hook (org-mode . org-cdlatex-mode)

      ;; comandos extras
      :config (add-to-list 'cdlatex-math-modify-alist '(?s "\\mathscr" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(?B "\\mathbb" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(?k "\\mathfrak" nil t nil nil))
              (add-to-list 'cdlatex-math-symbol-alist '(?* ("\\times" "\\otimes")))
              (add-to-list 'cdlatex-math-symbol-alist '(?d ("\\delta" "\\partial" "^{\\dag}")))
	      (add-to-list 'cdlatex-math-symbol-alist '(?> ("\\to" "\\longrightarrow")))
              (add-to-list 'cdlatex-math-symbol-alist '(?. ("\\cdot" "\\odot" "\\max")))
              (add-to-list 'cdlatex-math-symbol-alist '(?~ ("\\approx" "\\simeq" "\\propto")))
              (cdlatex-reset-mode))

(provide 'org-latex-config)
```

`org-link-config.el`
:   Customizes link behaviors for transclusion, subfigures in LaTeX, and
    integrated image pasting.

``` elisp
;; use subfigs in LaTeX export

;;  #+name: fig_name
;;  #+attr_latex: :options \centering
;;  #+caption: Figure Caption
;;  #+begin_figure 
;;  [[subfig:img1.png][Subcaption 1 >(scale=0.6)]] 
;;  [[subfig:img2.png][Subcaption 2 >(scale=0.6)]]
;;  [[subfig:img3.png][Subcaption 3 >(scale=0.6)]]
;;  #+end_figure
(with-eval-after-load 'org
  (org-link-set-parameters
   "subfig"
   :follow (lambda (file) (find-file file))
   :face '(:foreground "chocolate" :weight bold :underline t)
   :display 'full
   :export
   (lambda (file desc backend)
     (when (eq backend 'latex)
       (if (string-match ">(\\(.+\\))" desc)
           (concat "\\subfigure[" (replace-regexp-in-string "\s+>(.+)" "" desc) "]"
                   "{\\includegraphics"
                   "[" (match-string 1 desc) "]"
                   "{" file "}}")
         (format "\\subfigure[%s]{\\includegraphics{%s}}" desc file))))))

;; link files and display everything as a single file
(use-package org-transclusion
  :after org
  :bind (("C-c t m" . org-transclusion-transient-menu)
	 ("C-c t t" . org-transclusion-mode)))

;; org-paste-image, simpler than org-download
(defun org-paste-image ()
  "Paste an image from the clipboard, let the user choose a directory to save it,
and insert a link to it in the buffer. Supports Org-mode and LaTeX."
  (interactive)
  (let* ((session (getenv "XDG_SESSION_TYPE"))
         (command-header (cond
                          ((string= session "x11")
                           "xclip -selection clipboard -t image/png -o")
                          ((string= session "wayland")
                           "wl-paste --type image/png")
                          (t (error "Unknown session type: %s" session))))
         ;; choose target dir
         (base-dir (read-directory-name
                    "Choose the directory to save image: "
                    default-directory nil nil "img/"))
         (_ (make-directory base-dir t))
         (filename (concat (make-temp-name
                            (expand-file-name "img-" base-dir)) ".png"))
         (command (concat command-header " | convert - " (shell-quote-argument filename))))

    (shell-command command)
    (if (file-exists-p filename)
        (cond
         ((derived-mode-p 'org-mode)
          (insert (concat "[[file:" (file-relative-name filename default-directory) "]]"))
          (message "Saved image in %s and added link (Org-mode)"
                   (file-relative-name filename default-directory)))
         ((derived-mode-p 'latex-mode)
          (insert (format "\\begin{figure}[h]\n\\centering\n\\includegraphics[width=\\linewidth]{%s}\n\\caption{}\n\\label{fig:}\n\\end{figure}"
                          (file-relative-name filename default-directory)))
          (message "Saved image in %s and added link (LaTeX)"
                   (file-relative-name filename default-directory)))
         (t
          (message "Not in org-mode or LaTeX-mode" filename)))
      (message "Failed to save image"))))

;; add ~framed~ to C-c C-x s // C-c C-,
(add-to-list 'org-structure-template-alist '("f" . "framed"))

(provide 'org-link-config)
```

`org-ox-config.el`
:   Manages the export engine for converting Org files into LaTeX,
    Reveal.js presentations, and other formats.

``` elisp
;; org-export hook: if I have compiled the file once this session
;; it must redo it async each save
(defun my/org-export-and-set-async-hook ()
  (interactive)
  (org-export-dispatch)
  (setq-local local-org-export-dispatch-last-action
	      (append (list (car org-export-dispatch-last-action)
			    'async)
		      (if (eq (cadr org-export-dispatch-last-action) 'async)
			  (cddr org-export-dispatch-last-action)
			(cdr org-export-dispatch-last-action)))))

(defun my/org-export-run-on-save ()
  (interactive)
  (when (bound-and-true-p local-org-export-dispatch-last-action)
    (setq org-export-dispatch-last-action local-org-export-dispatch-last-action)
    (org-export-dispatch 1)))

(use-package org-reveal)
(use-package ox-reveal
  :after (org-reveal))
(load-library "ox-reveal")

(provide 'org-ox-config)
```

`org-config.el`
:   Aggregates all Org-related modules.

# AI

AI-related tooling and configuration.

`ai-config.el`
:   Integrates Large Language Models via `gptel`, with custom presets
    for coding, physics, and proofreading.

``` elisp
;; gptel
(use-package gptel
  :init
  ;; Avoid eager evaluation during byte-compilation (and before straight has
  ;; ensured the package is installed).
  (with-eval-after-load 'gptel
    (gptel-make-gemini "Gemini"
      :key (lambda ()
             (auth-source-pick-first-password
              :host "generativelanguage.googleapis.com"
              :user "apikey"))
      :stream t))
  :custom ((gptel-default-mode #'org-mode)
           (gptel-track-media t)
           (gptel-use-tools t)
           (gptel-model 'gemini-flash-latest)))

(with-eval-after-load 'gptel
  (require 'ai-presets-config))

(use-package gptel-commit
  :straight (:host github :repo "lakkiy/gptel-commit")
  :after (gptel magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . gptel-commit)))

(use-package gptel-agent
  :config (gptel-agent-update))

;; Keybindings (leader keys)
;; Keep this here (AI module) so the core keybindings remain generic.
(with-eval-after-load 'general
  ;; Prefer the leader definer provided by core, if present.
  (defvar nm/leader-keys nil
    "Leader key definer (general.el).")
  (defun nm/ai--leader-keys (&rest args)
    "Compatibility wrapper around the config's leader-key definer.

If `my/leader-keys` exists (defined in core), reuse it; otherwise create a
local leader definer.  ARGS are forwarded to `general-define-key` style
binding specs." 
    (if (fboundp 'my/leader-keys)
        (apply #'my/leader-keys args)
      (unless (fboundp 'nm/leader-keys)
        (general-create-definer nm/leader-keys
          :keymaps '(normal visual)
          :prefix "SPC"))
      (apply #'nm/leader-keys args)))

  (nm/ai--leader-keys
    ;; gptel
    "gg" #'gptel
    "ga" #'gptel-add
    "gm" #'gptel-menu
    "gr" #'gptel-rewrite
    "gs" #'gptel-send
    "gb" #'gptel-buffer
    "gq" #'gptel-abort
    "gc" #'gptel-commit))

(use-package gptel-org-tools
  :straight (:host codeberg :repo "bajsicki/gptel-got" :branch "main"))

(use-package ragmacs
   :straight (:host github :repo "positron-solutions/ragmacs":branch "master")
   :after gptel)

(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :config
  (mapc (apply-partially #'apply #'gptel-make-tool)
        (llm-tool-collection-get-all)))

(provide 'ai-config)
```

`ai-presets-config.el`
:   Defines custom gptel presets
    (code/physics/proofread/publication/etc.).

``` elisp
(gptel-make-preset 'code
  :description "Optimized for programming tasks; follows the current buffer's major mode/language."
  :backend "Gemini"
  :model 'gemini-3-flash-preview
  :system (string-join
           '("You are a senior software engineer and pair-programmer."
             "Primary goal: produce correct, idiomatic, maintainable code and precise explanations."
             "IMPORTANT: Always adapt to the language and conventions of the current buffer, inferred from its Emacs major mode and surrounding code."
             "Before changing anything, quickly read the relevant buffer region for context."
             "Ask clarifying questions when requirements are ambiguous; otherwise proceed."
             "When editing: make minimal, well-scoped changes; preserve style, formatting, and existing architecture."
             "When proposing code: include tests or usage examples when appropriate, and note edge cases."
             "When using tools: only use the subset of tools that best fit the current task and context; avoid irrelevant tools (e.g., do not use spell_check for code tasks unless explicitly requested)."
             "When unsure, state assumptions and provide alternatives.")
           "\n")
  :tools gptel--known-tools)

(gptel-make-preset 'physics
  :description "Optimized for studying physics and mathematics; clear derivations and careful reasoning."
  :backend "Gemini"
  :model 'gemini-3-pro-preview
  :system (string-join
           '("You are a physics and mathematics tutor and problem-solver."
             "Primary goal: help the user learn; prioritize clarity, correctness, and step-by-step derivations."
             "Use consistent notation; define variables and assumptions."
             "Check dimensional consistency and limiting cases when relevant."
             "Offer multiple solution paths (analytic, geometric, computational) when helpful."
             "If the user provides a problem statement in the buffer, read it and keep their notation."
             "When appropriate, summarize the key idea and common pitfalls at the end."
             "When using tools: only use the subset of tools that best fit the current task and context; avoid irrelevant tools.")
           "\n")
  :tools gptel--known-tools)

(gptel-make-preset 'proofread
  :description "Proofreading and copyediting in Portuguese and English; fixes grammar, style, and consistency."
  :backend "Gemini"
  :model 'gemini-2.5-flash
  :system (string-join
           '("You are an expert proofreader and copyeditor for Portuguese (PT-BR) and English (US/UK)."
             "Primary goal: correct grammar, spelling, punctuation, agreement, and style; improve clarity and flow without changing meaning."
             "Respect the author's voice; avoid unnecessary rewrites."
             "Detect and fix inconsistencies (tense, register, terminology, hyphenation, capitalization, citations)."
             "If the target variant is not specified, infer from the text and keep it consistent; ask if ambiguous."
             "When editing, keep formatting (Markdown/LaTeX/code blocks) intact; do not 'correct' code."
             "Provide corrected text; optionally list key changes if asked."
             "When using tools: only use the subset of tools that best fit the current task and context; avoid irrelevant tools.")
           "\n")
  :tools gptel--known-tools
  :temperature 0.2)

(gptel-make-preset 'publication
  :description "Scientific writing for publications: clarity, structure, rigor, and journal-ready style."
  :backend "Gemini"
  :model 'gemini-3-pro-preview
  :system (string-join
           '(
	     "You are an academic writing editor for scientific publications."
             "Primary goal: rewrite and refine text to be publication-ready while preserving meaning and technical correctness."
             "Improve structure (logical flow, paragraphing, topic sentences), concision, and readability."
             "Ensure rigorous claims: flag overstatements, missing definitions, unclear assumptions, and unsupported assertions."
             "Standardize terminology and symbols; keep notation consistent across the document."
             "Respect domain conventions; maintain LaTeX/Markdown formatting, citations, equations, figure/table references."
             "Do not change code blocks except for obvious typos when explicitly requested."
             "If the target venue/style is unspecified, default to a neutral academic tone; ask for journal/field guidelines if needed."
             "Output: provide an improved version of the text; if asked, provide a brief list of substantive changes."
             "When using tools: only use the subset of tools that best fit the current task and context; avoid irrelevant tools."
	     )
           "\n")
  :tools gptel--known-tools
  :temperature 0.2)

(gptel-make-preset 'physpub
  :description "Refines text for scientific journals, primarily in Physics, focusing on academic rigor, structural clarity, and technical precision."
  :backend "Gemini"
  :model 'gemini-3-pro-preview
  :system "You are an expert academic editor for high-impact scientific publications, with a primary focus on the field of Physics. Your objective is to refine text for clarity, logical flow, and technical rigor while maintaining the author's original meaning.

Guidelines:
- Structural Flow: Improve transitions, topic sentences, and the logical progression of arguments.
- Technical Rigor: Flag overstatements or unsupported claims. Ensure symbols, notation, and terminology (especially physical constants and units) are consistent and standard for the field.
- Tone: Maintain a neutral, formal, and concise academic style.
- Preservation: Do not alter LaTeX/Markdown formatting, citations, equations, or references. Do not modify code blocks.
- Output: Provide the improved version directly. If requested, include a brief list of substantive changes.
- Tooling: Use the provided tools only when strictly necessary to enhance the editing process."
  :temperature 0.2
  :tools gptel--known-tools)

(gptel-make-preset 'introspect
  :pre (lambda () (require 'ragmacs))
  :system
  "You are pair programming with the user in Emacs and on Emacs.
 
 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.
 
 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>
 
 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>
 
 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
  :tools '("introspection"))

(provide 'ai-presets-config)
```

``` elisp
(require 'org-core-config)
(require 'org-ui-config)
(require 'org-babel-config)
(require 'org-link-config)
(require 'org-agenda-config)
(require 'org-roam-config)
(require 'org-capture-config)
(require 'org-latex-config)
(require 'org-ox-config)

(provide 'org-config)
```

# Tools

General-purpose tools that integrate external applications and services
directly into the Emacs workflow.

`tools-completion-config.el`
:   Configures `corfu` for modern in-buffer completion and `eglot` for
    Language Server Protocol (LSP) support.

``` elisp
;; corfu para auto-complete
(use-package corfu
  :init (global-corfu-mode))
(provide 'tools-completion-config)

;; eglot para LSP
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (javascript-mode . eglot-ensure)
  (julia-mode . eglot-ensure)
  (java-mode . eglot-ensure))

(add-to-list 'exec-path "/home/nicolas/.local/bin")
(use-package eglot-booster
  :after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster" :branch "main")
  :config (eglot-booster-mode))

;; (use-package json-rpc)
```

`tools-references-config.el`
:   Manages academic bibliographies with `citar` and `ebib`, integrating
    PDFs and citations into the workflow.

``` elisp
;; i'm used to using ~org-ref~, but I'm going to migrate to ~citar~. 
;; the old config is going to be here

;; (use-package org-ref
;;   :hook (org-mode . (lambda () (require 'org-ref)))
;;   ;; :init (require 'org-ref-ivy)
;;   ;; :config (require 'org-ref-ivy)
;;   ;; (load-file "~/.config/emacs/straight/build/org-ref/org-ref-ivy.el") ;; talvez nao precise
;;   :config 
;;   (setq org-file-apps '((auto-mode . emacs)
;;                         (directory . emacs)
;;                         (\.mm\' . default)
;;                         (\.x?html?\' . default)
;;                         (\.pdf\' . emacs)))

;;   ;; :config (add-to-list 'org-latex-classes
;;   ;;                      '("uspsc"
;;   ;;                        "\\documentclass{USPSC-classe/USPSC}"
;;   ;;                        ("\\chapter{%s}" . "\\chapter*{%s}")
;;   ;;                        ("\\section{%s}" . "\\section*{%s}")
;;   ;;                        ("\\subsection{%s}" . "\\subsection*{%s}")
;;   ;;                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;   ;;                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;   :bind (("C-c C-]" . org-ref-insert-link)
;;          ("C-c )" . org-ref-insert-ref-link)))
;;
;; (use-package org-roam-bibtex
;;     :ensure t
;;     :after org-roam
;;     :load-path org-roam-directory
;;     :config (require 'org-ref)
;;     :init (org-roam-bibtex-mode))

(use-package citar
  :custom
  (citar-bibliography (list my/bibliography-file))
  (citar-notes-paths (list org-roam-directory))
  (citar-library-paths (list my/pdf-library))
  :hook ((LaTeX-mode . citar-capf-setup)
	 (org-mode . citar-capf-setup)))

;; not sure if this should be on
(use-package citar-org-roam
  :after (citar org-roam)
  :custom (citar-org-roam-capture-template-key "r"))

(citar-org-roam-mode)

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my/bibliography-file)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path my/pdf-library))

;; useful to look for papers, never really used it
(use-package biblio
  :demand t)

(use-package biblio-openlibrary
  :straight (:host github :repo "fabcontigiani/biblio-openlibrary" :branch "master"))
(use-package biblio-gbooks
  :straight (:host github :repo "jrasband/biblio-gbooks" :branch "main"))

(defun my/ebib-reading-list-add-org-cite ()
  "Add an Org-cite citation to the newly created Ebib reading-list item."
  (let ((key (ebib--get-key-at-point)))
    (with-current-buffer (find-file-noselect ebib-reading-list-file)
      ;; Ebib deixa point no item recém-criado neste buffer.
      ;; Então inserimos uma linha de citação logo após o heading/properties.
      (save-excursion
        (org-back-to-heading t)
        ;; pula drawer de PROPERTIES, se existir
        (forward-line 1)
        (when (looking-at-p ":PROPERTIES:")
          (re-search-forward "^:END:[ \t]*$" nil t)
          (forward-line 1))
        (insert (format "- [cite:@%s]\n" key)))
      (save-buffer))))

;; bibliography tool
(use-package ebib
  :after biblio

  :custom ((ebib-default-directory my/library-directory)
	   (ebib-bib-search-dirs (file-name-concat my/library-directory "pdfs"))
	   (ebib-preload-bib-files `(,my/bibliography-file))
	   (ebib-import-source-directory ebib-bib-search-dirs)
	   (ebib-reading-list-file (file-name-concat ebib-default-directory "reading-list.org")))
  :config
  (require 'ebib-biblio)
  (define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import))

(add-hook 'ebib-reading-list-new-item-hook #'my/ebib-reading-list-add-org-cite)

(provide 'tools-references-config)
```

`tools-utils-config.el`
:   Houses essential utilities like Magit for Git, PDF-Tools for
    document viewing, and Jinx for spellchecking.

``` elisp
;; box instead of dedicated buffer
(use-package eldoc-box)

;; expand-region increases region by semantic expressions
(use-package expand-region
    :bind ("C-=" . er/expand-region))

;; hide-show-mode folds code
;; in evil:
;; - zr (show-all)
;; - zm (fold-all)
;; - zo (show-this)
;; - zc (fold-this)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; elfeed
(use-package elfeed
  :custom (elfeed-db-directory "~/.local/share/elfeed")
  :bind ("C-c e" . elfeed))

(use-package elfeed-org
  :straight (:host github :repo "remyhonig/elfeed-org" :branch "master")
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (list (file-name-concat org-directory "elfeed.org"))))

;;jinx
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :init (setq jinx-languages "en_US pt_BR")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
(add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
;;ledger
(use-package ledger-mode
  :custom
  ((ledger-reports
    (quote
     (("bal cf" "%(binary) -f %(ledger-file) bal ^Income ^Expenses --real")
      ("bal nw" "%(binary) -f %(ledger-file) bal ^Assets ^Liabilities --real")
      ("bal precos" "%(binary) -f %(ledger-file) --price-db btc_price.db --price-db comm.db -V bal")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("reg - price" "%(binary) -f %(ledger-file) reg -V --price-db btc_price.db --price-db comm.db")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
   (ledger-post-amount-alignment-column 60))
  :hook
  (ledger-mode . (lambda ()
                   (setq-local tab-always-indent 'complete)
                   (setq-local completion-ignore-case t)
                   (setq-local ledger-complete-in-steps t))))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge)

;; email
(use-package notmuch
  :custom ((notmuch-search-oldest-first nil)
	   (send-mail-function 'smtpmail-send-it)
	   (message-send-mail-function 'smtpmail-send-it)
	   (user-mail-address "nicolas.morazotti@gmail.com")
	   (smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil)))
	   (smtpmail-default-smtp-server "smtp.gmail.com")
	   (smtpmail-smtp-server "smtp.gmail.com")
	   (smtpmail-smtp-service 587)
	   (smtpmail-debug-info t)
	   (starttls-extra-arguments nil)
	   (starttls-gnutls-program "/usr/bin/gnutls-cli")
	   (starttls-extra-arguments nil)
	   (starttls-use-gnutls t)
	   (message-signature-file
	    (file-name-concat user-emacs-directory "private" "signature")))
  :bind ("C-c m" . notmuch))

;; pdf-tools
(use-package pdf-tools
  :hook (pdf-view-mode . blink-cursor-mode)
  
  :config
  (define-key pdf-view-mode-map (kbd ":") 'pdf-view-goto-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (define-key pdf-view-mode-map (kbd "/") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "?") 'isearch-backward)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "J") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "K") 'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "TAB") 'pdf-outline)
  (define-key pdf-view-mode-map (kbd "C-S-r") 'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "T") 'pdf-view-themed-minor-mode)
  (define-key pdf-view-mode-map (kbd "s") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "a") 'pdf-view-fit-page-to-window)
  (pdf-loader-install)

  :mode ("\\.pdf" . pdf-view-mode)

  :custom (pdf-view-midnight-colors '("#FFBB33" . "#222222")))

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

;; terminal
;; I was using ~toggle-term~ to access popup terminal, but it was kinda clunky
;; apparently there's a 'popper' thingy to recover such behavior
(use-package popper
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("*vterm*"
	  "*eshell*"
	  "\\*Messages\\*"
	  "\\*Warning\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vterm)

;;vundo
(use-package vundo
  :demand t)

;; winner mode allows C-c <left> / C-c <right> to recover previous window configuration
(winner-mode)

;; tree-sitter
(use-package tree-sitter
  :custom (treesit-language-source-alist
	   '((python "https://github.com/tree-sitter/tree-sitter-python")
             (clojure  "https://github.com/tree-sitter/tree-sitter-clojure")
             (julia  "https://github.com/tree-sitter/tree-sitter-julia")
	     (bash   "https://github.com/tree-sitter/tree-sitter-bash")
             (c      "https://github.com/tree-sitter/tree-sitter-c")
             (cpp    "https://github.com/tree-sitter/tree-sitter-cpp"))))

;; split and follow window
(defun my/split-and-follow (direction)
  (interactive "c")
  (catch 'my-tag
    (cond ((= direction ?h) (split-window-below))
  	((= direction ?v) (split-window-right))
  	(t (throw 'my-tag "no correct character pressed")))
    (other-window 1)))

(defun my/split-and-follow-horizontally ()
  (interactive)
  (my/split-and-follow ?h))

(defun my/split-and-follow-vertically ()
  (interactive)
  (my/split-and-follow ?v))

;; zoom-window, just like <leader>-z on tmux
(use-package zoom-window
  :bind (("M-z" . zoom-window-zoom)
	 ("C-M-z" . zoom-window-next)))

;; (defvar zoom-register ?z
;;   "the register to store the window configuration for zooming/unzooming.")

;; (defvar zoomed-in-p nil
;;   "a flag to track if the window is currently zoomed in.")

;; (defun toggle-zoom-window ()
;;   "toggle zooming the current window: maximize or restore."
;;   (interactive)
;;   (if zoomed-in-p
;;       (restore-window-configuration-from-zoom-register)
;;     (save-window-configuration-to-zoom-register)
;;     (delete-other-windows))
;;   (setq zoomed-in-p (not zoomed-in-p)))

;; (defun save-window-configuration-to-zoom-register ()
;;   "save the current window configuration to a register for zooming/unzooming."
;;   (window-configuration-to-register zoom-register)
;;   (message "Window configuration saved for zooming."))

;; (defun restore-window-configuration-from-zoom-register ()
;;   "restore the window configuration from the zoom register."
;;   (jump-to-register zoom-register)
;;   (message "Window configuration restored."))

;; (global-set-key (kbd "M-z") 'toggle-zoom-window)

(provide 'tools-utils-config)
```

`tools-debug-config.el`
:   Debugger integration.

``` elisp
(use-package dape)
(provide 'tools-debug-config)
```

`tools-config.el`
:   Aggregates all tools modules.

``` elisp
(require 'tools-references-config)
(require 'tools-utils-config)
(require 'tools-debug-config)
(require 'tools-completion-config)

(provide 'tools-config)
```

# Programming Languages

Language-specific configurations that provide specialized editing
features, REPLs, and formatting tools.

`langs-python-config.el`
:   Python setup including virtual environment management and black
    formatting.

``` elisp
(use-package python-mode
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))


(use-package python-black)
(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(provide 'langs-python-config)
```

`langs-julia-config.el`
:   Comprehensive Julia support with an integrated REPL and specialized
    keybindings.

``` elisp
(use-package julia-repl)
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :interpreter ("julia" . julia-mode)
  :bind
  (:map julia-mode-map
        ("C-c C-z" . julia-repl)
        ("C-c C-c" . julia-repl-send-buffer)
        ("C-<return>" . julia-repl-send-region-or-line)
        ("C-<enter>" . julia-repl-send-region-or-line)
        ("C-c C-d" . julia-repl-doc)))

(provide 'langs-julia-config)
```

`langs-latex-config.el`
:   Full LaTeX environment using AUCTeX, Synctex, and mathematical
    calculation helpers.

``` elisp
;; auctex enhances usability in latex
(use-package auctex
  :init (require 'latex)
  :custom ((TeX-source-correlate-mode t)
	   (TeX-source-correlate-method-active 'synctex))
  :bind (:map LaTeX-mode-map
	      ("C-S-e" . latex-math-from-calc)
  	      :map org-mode-map
	      ("C-S-e" . latex-math-from-calc))

  :config
  (setq-default TeX-master nil)
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))
  :custom ((TeX-auto-save t)
	   (TeX-parse-self t)
	   (font-latex-fontify-script nil)))

(use-package latex-extra)

;; latex references manager -- not really using latex much tho
(use-package reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (reftex-default-bibliography (list my/bibliography-file))
  :hook ((TeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . LaTeX-math-mode)
	 (TeX-mode . outline-minor-mode)
	 (TeX-mode . turn-on-reftex)))

(provide 'langs-latex-config)
```

`langs-clojure-config.el`
:   Clojure development tools utilizing Tree-sitter and the CIDER
    environment.

``` elisp
(use-package clojure-ts-mode)

(use-package cider)
(provide 'langs-clojure-config)
```

`langs-utils-config.el`
:   Shared programming utilities (parens management, snippets, etc.).

``` elisp
;; better parenthesis
(use-package smartparens
  :hook ((org-mode latex-mode C-mode julia-mode python-mode) . smartparens-mode))

(use-package paredit
  :hook ((snippet-mode lisp-mode lisp-interaction-mode clojure-mode emacs-lisp-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode LaTeX-mode latex-mode org-mode) . rainbow-delimiters-mode))

;; mise
;; helps with binaries installed with mise
(use-package mise
  :config (mise-mode))

;; snippets
;; actually, I'm checking tempel, and it looks good.
;; (add-to-list 'load-path
;;              (file-name-concat user-emacs-directory "plugins" "yasnippet"))
(use-package yasnippet
  :init (yas-global-mode))

(use-package yasnippet-snippets)

;; yasnippet auto load
;; add symbol 'auto on condition

(defun my/yas-try-expanding-auto-snippets ()
  (when (bound-and-true-p yas/minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))

(add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

(defun my/yas-math-modify-accent (text)
  (let ((argument t))
    (condition-case nil
	(progn
          (backward-sexp)
          (kill-sexp)
          (delete-char 1))
      (error (setq argument 'nil)))
    (insert "\\" text "{" (if argument (current-kill 0) "") "}")))

(use-package resnippets
  :straight (:host github :repo "morazotti/resnippets" :branch "master")
  ;; :custom (resnippets-expand-env
  ;;          '((smartparens-mode . nil)
  ;;            (cdlatex-mode . nil)))
  :hook (org-mode . resnippets-mode))

(with-eval-after-load 'resnippets
  (require 'langs-resnippets-config))


;; remap -mode to -ts-mode
(add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


(provide 'langs-utils-config)
```

`langs-resnippets-config.el`
:   Resnippets definitions used by the shared snippets layer.

``` elisp
(resnippets-define
   "math-mode"
   '(:mode (LaTeX-mode org-mode)
     :condition (or (texmathp) (org-inside-LaTeX-fragment-p)))
   ("\\([a-zA-Z\\]+\\)hat" '("\\hat{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)bar" '("\\bar{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)til" '("\\tilde{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)vec" '("\\vec{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)<-" '("\\overleftarrow{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)cnc" '("\\cancel{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)dag" '(1 "^{\\dag}"))
   ("\\([a-zA-Z{\\]+\\)hh" '(1 "\\hbar"))
   ("\\([a-zA-Z{\\]+\\)ll" '(1 "\\ell"))
   ("\\([a-zA-Z\\]+\\)conj" '(1 "^{*}"))
   ("\\([a-zA-Z\\]+\\)trans" '(1 "^{T}"))
   ("\\([a-zA-Z\\]+\\)inv" '(1 "^{-1}"))

   ("<\\([a-zA-Z0-9_^{}\\\\]+\\)|" '("\\bra{" 1 "}"))
   ("<\\([a-zA-Z0-9_^{}\\\\]+\\) ?|\\([a-zA-Z0-9_^{}\\\\]+\\) ?>"
    '("\\braket{" 1 "}{" 2 "}"))

   ("\\([\(]?\\)\\([ ]?\\)//" '(1 "\\frac{" (resnippets-cursor) "{}"))
   ("\\([a-zA-Z0-9{}\\\\]+\\)/" '("\\frac{" 1 "}{" (resnippets-cursor)))
   ("\\([a-zA-Z}]\\)\\([0-9]\\)" '(1 "_" 2))
   ("_\\([0-9][0-9]\\)" '("_{" 1 (resnippets-cursor) "}"))

   ;; fun call
   ;; any elisp function ~fun~ with a numeric argument
   ;; can be called by ;fun=argument;
   (";\\([^=]+\\)=\\([\-0-9.]+\\);"
    '((number-to-string
       (funcall (intern (resnippets-group 1))
                (string-to-number (resnippets-group 2)))))))

(provide 'langs-resnippets-config)
```

`langs-config.el`
:   Aggregates all language modules.

``` elisp
(require 'langs-utils-config)
(require 'langs-latex-config)
(require 'langs-python-config)
(require 'langs-clojure-config)
(require 'langs-julia-config)

(provide 'langs-config)
```
