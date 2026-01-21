This is a modular Emacs configuration designed for performance, scientific writing, and a hybrid Vim/Emacs workflow. The setup is divided into functional modules to maintain clarity and ease of maintenance.


# Initialization

The startup process begins with the early-init file to optimize the UI and memory management, followed by the main init file which orchestrates the loading of all sub-modules.

-   **`early-init.el`:** Configures the frame UI before it's created and maximizes garbage collection thresholds for a faster startup.

<div class="nil" id="org9283718">
<p>
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)
</p>

</div>

-   **`init.el`:** The main entry point. It sets up the load paths, initializes modules, and restores editor performance settings after startup.

<div class="nil" id="org5c5d0f0">
<p>
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
</p>

<p>
(let ((file-name-handler-alist-original file-name-handler-alist))
  (setq file-name-handler-alist nil)
</p>

<p>
(when (file-exists-p custom-file) (load custom-file))
</p>

<p>
(dolist (module '("core" "org" "tools" "langs"))
  (add-to-list 'load-path
	       (expand-file-name
		(format "modules/%s" module)
		user-emacs-directory)))
</p>

<p>
;; if we ever need to M-x straight-freeze-versions, we need to uncomment the following line before
;; (load (expand-file-name "modules/core/core-packages-config.el" user-emacs-directory))
</p>

<p>
(require 'core-config)
(require 'org-config)
(require 'tools-config)
(require 'langs-config)
</p>

<p>
;; (require 'wip-functions)
</p>

<p>
(setq file-name-handler-alist file-name-handler-alist-original))
</p>

<p>
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
</p>

</div>

-   **`emacs-custom.el`:** Stores variables and faces generated through the Emacs Customization UI, kept separate to prevent cluttering the main configuration.

<div class="nil" id="orgc5b9486">
<p>
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(ebib-reading-list-template "* %M %T\12:PROPERTIES:\12%K\12:END:\12")
 '(org-hide-emphasis-markers t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-safe-remote-resources '("\\`\\[\\[<a href="early-init\\.el">early-init\\.el</a>]]\\'"))
 '(org-use-sub-superscripts nil))
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
</p>

</div>


# Core Configuration

These modules define the fundamental editor experience, from package management to the visual theme and keybinding philosophy.

-   **`core-packages-config.el`:** Bootstraps `straight.el` as the package manager and sets up `use-package` for declarative configuration.

<div class="nil" id="org4f052db">
<p>
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
         "<a href="https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el">https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el</a>"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
</p>

<p>
;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
</p>

<p>
(use-package project
  :straight t
  :ensure t)
</p>

<p>
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))
</p>

<p>
(provide 'core-packages-config)
</p>

</div>

-   **`core-variables-config.el`:** Centralizes global settings, directory paths for backups/undo history, and environment variables.

<div class="nil" id="orgd73f196">
<p>
(defconst home (expand-file-name "~"))
(setq-default abbrev-mode t)
(setq my/library-directory (file-name-concat home "Dropbox" "my<sub>library</sub>/")
      my/bibliography-file (file-name-concat my/library-directory "referencias.bib")
      my/pdf-library (file-name-concat my/library-directory  "pdfs")
      warning-minimum-level :emergency
      org-roam-directory (file-name-concat home "Documents" "roam")
      ispell-dictionary "pt<sub>BR</sub>"
      fill-column 72
</p>

<p>
;; authinfo
auth-sources '("~/.authinfo.gpg" "~/.authinfo")
</p>

<p>
make-backup-files t
backup-by-copying t              ; evita problemas com links/permissões
version-control t
delete-old-versions t
kept-old-versions 2
kept-new-versions 10
backup-directory-alist `(("." . ,(expand-file-name "~/.cache/emacs/backups/")))
backup-inhibited nil
</p>

<p>
;; autosaves
auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.cache/emacs/autosaves/") t))
auto-save-default t
</p>

<p>
;; undo history
undo-limit        800000
undo-strong-limit 12000000
undo-auto-save-history t
undo-directory-alist `(("." . "~/.cache/emacs/undo/")))
</p>

<p>
(setenv "SSH<sub>AUTH</sub><sub>SOCK</sub>" (format "%s/ssh-agent.socket" (getenv "XDG<sub>RUNTIME</sub><sub>DIR</sub>")))
(put 'narrow-to-region 'disabled nil)
(with-current-buffer "<b>scratch</b>"
  (emacs-lock-mode 'kill))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)
</p>

<p>
(provide 'core-variables-config)
</p>

</div>

-   **`core-ui-config.el`:** Configures the visual interface, including the theme, fonts for both fixed and variable pitch, ligatures, and the modeline.

<div class="nil" id="orgd3b9350">
<p>
(setq global-hl-line-mode t
      visual-bell t
      show-paren-mode t)
(set-fringe-mode 10)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
</p>

<p>
(defvar my/fixed-font "Ligamonacop")
(defvar my/variable-font "CMU Serif")
(defvar my/fixed-font-height 120)
(defvar my/variable-font-height 140)
</p>

<p>
(defun my/setup-fonts (&amp;optional frame)
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
</p>

<p>
(add-hook 'after-make-frame-functions #'my/setup-fonts)
(my/setup-fonts)
</p>

<p>
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font my/fixed-font
                      :height 120))
</p>

<p>
(when (display-graphic-p)
  (set-face-attribute 'variable-pitch nil
                      :font my/variable-font
                      :height 140))
</p>

<p>
;; old
;; (setq default-font "Ligamonacop 12")
;; (set-frame-font default-font nil t)
;; (setq default-frame-alist '((font . "Ligamonacop 12")))
</p>

<p>
;; all-the-icons
;; (use-package all-the-icons
;;   :straight (:host github :repo "domtronn/all-the-icons.el" :branch "master"))
</p>

<p>
;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))
</p>

<p>
;; theme
(use-package base16-theme)
;; (use-package doom-themes)
;; (use-package modus-themes
;;   :straight (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(defvar my/theme 'base16-catppuccin-latte) ;;anterior: 'doom-tokyo-night
(load-theme my/theme t nil)
</p>

<p>
;; visual-shorthands-mode - hide long prefixes made easy
(use-package visual-shorthands-mode
  :straight (:host github
	     :repo "morazotti/visual-shorthands.el"
	     :branch "feat/region-as-default-longhand")
  :config (global-visual-shorthands-mode 1))
</p>

<p>
;; ligature
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'org-mode '("|||&gt;" "&lt;|||" "&lt;==&gt;" "&lt;!&ndash;" "####" "<code>~&gt;" "***" "||=" "||&gt;"
                                      ":::" "::=" "=:=" "===" "==&gt;" "=!=" "=&gt;&gt;" "=&lt;&lt;" "=/=" "!=="
                                      "!!." "&gt;=&gt;" "&gt;&gt;=" "&gt;&gt;&gt;" "&gt;&gt;-" "&gt;-&gt;" "-&gt;&gt;" "--&gt;" "---" "-&lt;&lt;"
                                      "&lt;~</code>" "&lt;~&gt;" "&lt;*&gt;" "&lt;||" "&lt;|&gt;" "&lt;\(>" "<==" "<=>" "<=<" "<->"
                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                      "..." "+++" "/==" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                      "[|" "]#" "::" ":=" ":>" ":<" "\)&gt;" "<code>=" "=&gt;" "!</code>" "!!" "&gt;:"
                                      "&gt;=" "&gt;&gt;" "&gt;-" "-<code>" "-|" "-&gt;" "--" "-&lt;" "&lt;</code>" "&lt;*" "&lt;|" "&lt;:"
                                      "&lt;\(" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "__" "~~" "(*" "*)"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<\)&gt;" "&lt;==" "&lt;=&gt;" "&lt;=&lt;" "&lt;-&gt;"
                                       "&lt;&ndash;" "&lt;-&lt;" "&lt;&lt;=" "&lt;&lt;-" "&lt;&lt;&lt;" "&lt;+&gt;" "&lt;/&gt;" "###" "#<sub>(" "..&lt;"
                                       "&hellip;" "<del>+</del>" "<i><code>=" "_|_" "www" "&amp;&amp;" "^</code>" "<code>~" "~@" "~="
                                       "~&gt;" "</code>-" "<b>*" "*&gt;" "</b></i>" "||" "|}" "|]" "|=" "|&gt;" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":&gt;" ":&lt;" "\(>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<\)" "&lt;=" "&lt;&gt;" "&lt;-" "&lt;&lt;" "&lt;+" "&lt;/" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "<del>&gt;" "+</del>" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/&gt;" "__" "~~" "(<b>" "</b>)"))</sub>)
(global-ligature-mode)
</p>

<p>
;; doom modeline
(use-package doom-modeline
  :straight (:host github :repo "seagle0128/doom-modeline" :branch "master")
  :custom ((doom-modeline-buffer-file-name-style 'buffer-name)
	   (doom-modeline-buffer-encoding nil))
  :config (add-hook 'after-make-frame-functions (lambda (frame)
                                                  (setq doom-modeline-icon t))))
(doom-modeline-mode)
</p>

<p>
(provide 'core-ui-config)
</p>

</div>

-   **`core-keybindings-config.el`:** Implements `evil-mode` for Vim emulation and defines a central leader-key system using `general.el`.

<div class="nil" id="orgf49ccc6">
<p>
(defun my/duplicate-line ()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (insert "\n" line))))
</p>

<p>
(global-set-key (kbd "&lt;f5&gt;") 'revert-buffer)
(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)
(global-set-key (kbd "C-,") 'my/duplicate-line)
</p>

<p>
;; vim-keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
</p>

<p>
:config
(define-key evil-visual-state-map (kbd "=") 'er/expand-region)
(define-key evil-visual-state-map (kbd "-") 'er/contract-region)
</p>

<p>
  :hook
  (delve-mode . turn-off-evil-mode))
(evil-mode)
</p>

<p>
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
</p>

<p>
(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))
</p>

<p>
(use-package evil-mc
  :demand t
  :after evil
  :init (global-evil-mc-mode 1))
</p>

<p>
;; leader-key
(use-package general
    :config (general-evil-setup t)
    (general-create-definer my/leader-keys
      :keymaps '(normal visual)
      :prefix "SPC"))
</p>

<p>
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
</p>

<p>
;; commands
"x" 'execute-extended-command
":" 'eval-expression
";" 'avy-goto-line
"j" 'my/duplicate-line
</p>

<p>
;; movement
"TAB" 'other-window
"s" 'avy-goto-char
</p>

<p>
;; jinx
"tb" 'jinx-correct-all
"tw" 'jinx-correct-nearest
</p>

<p>
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
</p>

<p>
;; magit
"gg" 'magit-status
;; "gb" 'magit-blame
"gc" 'magit-commit
"gC" 'magit-clone
"gp" 'magit-push
"gR" 'magit-revert
"gs" 'magit-stage 
"gx" 'magit-reset
"gz" 'magit-stash
</p>

<p>
;; open - general
"om" 'notmuch
"of" 'find-file-other-window
"o," 'consult-buffer-other-window
</p>

<p>
;; project.el
"pf" 'project-find-file
"pb" 'consult-project-buffer
"pc" 'project-compile
"pp" 'project-switch-project
"pk" 'project-kill-buffers
"ps" 'project-shell
"p!" 'project-shell-command
</p>

<p>
;; help
"ha" 'apropos-command
"hf" 'describe-function
"hk" 'describe-key
"hv" 'describe-variable
"hd" 'eldoc
"h." 'eldoc-box-help-at-point
</p>

<p>
;; register
"rs" 'consult-register-store
"rl" 'consult-register-load
"rr" 'consult-register
</p>

<p>
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
</p>

<p>
;; consult
"/"  'consult-ripgrep
"cd" 'consult-find
"cG" 'consult-git-grep
</p>

<p>
;; org ref
"[" 'citar-open
"]" 'org-cite-insert
")" 'consult-reftex-insert-reference
</p>

<p>
;; snippets
"yi" 'yas-insert-snippet
"yn" 'yas-new-snippet
"yv" 'yas-visit-snippet-file
</p>

<p>
;; agenda
"aa" 'org-agenda
"a[" 'org-agenda-file-to-front
"a]" 'org-remove-file
</p>

<p>
;; store link
"ls" 'org-store-link
"li" 'org-insert-link
</p>

<p>
;; smart-parens
"(k" 'sp-unwrap-sexp
"((" 'sp-rewrap-sexp
</p>

<p>
;; ;; multiple cursors
;; "@" 'evil-multiedit-toggle-marker-here
;; "m@" 'evil-multiedit-match-all
;; "mn" 'evil-multiedit-match-and-next
;; "mp" 'evil-multiedit-match-and-prev
</p>

<p>
;; narrow
"ns" 'org-narrow-to-subtree
"nn" 'narrow-to-region
"np" 'narrow-to-page
"nd" 'narrow-to-defun
"nw" 'widen
</p>

<p>
;; vundo
"u" 'vundo
</p>

<p>
;; elfeed
"ee" 'elfeed
</p>

<p>
;; ebib
"eb" 'ebib
</p>

<p>
;; terminal
"tt" 'vterm
"et" 'eshell
</p>

<p>
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
</p>

<p>
;; other commands
"ie" 'emoji-search
"ip" 'org-paste-image
"ev" 'org-babel-tangle
"ex" 'org-export-dispatch)
</p>

<p>
(provide 'core-keybindings-config)
</p>

</div>

-   **`core-utils-config.el`:** Manages the completion stack (Vertico, Marginalia, Orderless) and essential navigation tools like Consult and Embark.

<div class="nil" id="org95ae5a3">
<p>
;; Ibuffer
(use-package ibuffer
    :bind  ("C-x C-b" . ibuffer)
    :config (setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("dired" (mode . dired-mode))
                   ("org" (mode . org-mode))
</p>

<p>
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
			   (name . "^.*otmuch.*\(")
                               (mode . notmuch-hello-mode)
                               (mode . notmuch-search-mode)
                               (mode . notmuch-show-mode)
                               (mode . notmuch-tree-mode)
                               (mode . notmuch-message-mode)))
                   ("ledger" (or (mode . ledger-mode) (mode . ledger-report-mode)))
                   ("emms" (name . "^\\*EMMS.*"))
                   ("emacs" (or
                             (name . "^\\*scratch\\*\)")
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
</p>

<p>
;; ace-window - avy for windows
(use-package ace-window
  :custom (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  :bind (("C-x o" . other-window)
	 ("M-o" . ace-window)))
</p>

<p>
;; avy
(use-package avy
    :config (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    :bind (("M-s M-s" . avy-goto-char)
           ("M-g M-g" . avy-goto-line)))
</p>

<p>
;; ivy - some things need it
;; (use-package ivy
;;   :demand t
;;   :config
;;   (ivy-mode 1)
;;   (ivy-mode -1))
</p>

<p>
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
</p>

<p>
;; Enable automatic preview at point in the <b>Completions</b> buffer. This is
;; relevant when you use the default completion UI.
:hook (completion-list-mode . consult-preview-at-point-mode)
</p>

<p>
;; The :init configuration is always executed (Not lazy)
:init
</p>

<p>
;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)
</p>

<p>
;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)
</p>

<p>
;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
</p>

<p>
;; Configure other variables and modes in the :config section,
;; after lazily loading the package.
:config
</p>

<p>
;; Optionally configure preview. The default value
;; is 'any, such that any key triggers the preview.
;; (setq consult-preview-key 'any)
;; (setq consult-preview-key "M-.")
;; (setq consult-preview-key '("S-&lt;down&gt;" "S-&lt;up&gt;"))
;; For some commands and buffer sources it is useful to configure the
;; :preview-key on a per-command basis using the `consult-customize' macro.
(consult-customize
 consult-theme :preview-key '(:debounce 0.4 any)
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult&ndash;source-bookmark consult&ndash;source-file-register
 consult&ndash;source-recent-file consult&ndash;source-project-recent-file
 ;; :preview-key "M-."
 :preview-key '(:debounce 0.4 any))
</p>

<p>
;; Optionally configure the narrowing key.
;; Both &lt; and C-+ work reasonably well.
(setq consult-narrow-key "&lt;") ;; "C-+"
</p>

<p>
;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
</p>

<p>
;; By default `consult-project-function' uses `project-root' from project.el.
;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
;; (setq consult-project-function #'consult&ndash;default-project&ndash;function)
  ;;;; 2. vc.el (vc-root-dir)
;; (setq consult-project-function (lambda (<span class="underline">) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
;; (setq consult-project-function (lambda (</span>) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
;; (setq consult-project-function nil)
)
</p>

<p>
(use-package consult-reftex
  :after consult
  :straight (:host github :repo "karthink/consult-reftex" :branch "master"))
</p>

<p>
(use-package embark
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
</p>

<p>
:init
;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)
</p>

<p>
;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;; strategy, if you want to see the documentation from multiple providers.
(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
</p>

<p>
:config
;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
	     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
	       nil
	       (window-parameters (mode-line-format . none))))
</p>

<p>
;; jinx
(keymap-set jinx-repeat-map "RET" 'jinx-correct)
(embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
(add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
(add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
(add-to-list 'embark-repeat-actions #'jinx-next)
(add-to-list 'embark-repeat-actions #'jinx-previous)
(add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark&ndash;ignore-target))
(add-to-list 'embark-default-action-overrides (list jinx #'jinx-correct)))
</p>

<p>
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
</p>

<p>
;; vertico - marginalia - orderless: minibuffer framework
(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
</p>

<p>
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
</p>

<p>
(use-package marginalia
  :init (marginalia-mode))
</p>

<p>
(use-package orderless
    :custom
    (completion-styles '(orderless basic flex))
    (completion-category-overrides '((file (styles basic partial-completion)))))
</p>

<p>
;; wgrep - allows changing grep buffers
(use-package wgrep)
;;(require 'wgrep)
</p>

<p>
(provide 'core-utils-config)
</p>

</div>


# Org Mode

Org-mode is the heart of this configuration, tailored for academic research, note-taking, and scientific publishing.

-   **`org-core-config.el`:** Sets up the base Org-mode environment, utilizing a specific development branch for enhanced LaTeX previews.

<div class="nil" id="org4c11f2f">
<p>
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
            (call-process "git" nil t nil "rev-parse" "&ndash;short" "HEAD")
            (buffer-string)))))
    (with-temp-file "org-version.el"
      (insert
       (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
       (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
       "(provide 'org-version)\n"))))
</p>

<p>
(use-package org
  :straight (:host nil
             :repo "<a href="https://git.tecosaur.net/tec/org-mode.git">https://git.tecosaur.net/tec/org-mode.git</a>"
             :branch "dev"
             :remote "tecosaur"
             :files (:defaults "etc")
             :build t
             :pre-build (my/org-generate-version-file))
  :demand t
  :init
  (setq org-directory (file-name-concat home "Documents" "org"))
</p>

<p>
:custom ((org-src-fontify-natively t)
	 (org-src-window-setup 'reorganize-frame)
	 (org-confirm-babel-evaluate nil)
</p>

<p>
;; org-cite
(org-cite-global-bibliography (list my/bibliography-file))
(org-cite-insert-processor 'citar)
(org-cite-follow-processor 'citar)
(org-cite-activate-processor 'citar)
</p>

<p>
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
	       :matchers ("begin" "\(1" "\)" "$$" "\\(" "\\[" )))
</p>

<p>
(org-preview-latex-image-directory
 (file-name-concat home ".cache" "ltximg"))
</p>

<p>
;; ox
(org-export-async-init-file (expand-file-name "init-async.el" user-emacs-directory)))
</p>

<p>
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
       (:map org-mode-map ("&lt;f7&gt;" . my/org-export-and-set-async-hook))))
</p>

<p>
;; some extra configs to org and org-export
(use-package org-contrib)
(use-package ox-extra
  :after (org org-contrib)
  :config (ox-extras-activate '(latex-header-blocks ignore-headlines)))
</p>

<p>
(provide 'org-core-config)
</p>

</div>

-   **`org-agenda-config.el`:** Manages task lists and custom agenda views for project tracking and personal organization.

<div class="nil" id="org8cddf6b">
<p>
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/tasks.org")
                             (concat org-directory "/opusdei.org")))
(global-set-key (kbd "C-c a") 'org-agenda)
</p>

<p>
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current) subtree-end nil)))
</p>

<p>
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
</p>

<p>
;; super agenda - not sure if I should use it again
;; (use-package org-super-agenda
;;   :init (org-super-agenda-mode 1)
;;   ;; :init (setq initial-buffer-choice (lambda () (get-buffer "<b>Org Agenda</b>")))
;;   :config (setq org-super-agenda-groups
;;                 '(
;;                   (:auto-property "ProjectId")
</p>

<p>
;;                   ;; (:name "blackgenn"
;;                   ;;        :scheduled today)
;;                   (:name "emacs" :tag "emacs")
;;                   (:name "books"
;;                          :and (:tag "book")))))
</p>

<p>
(provide 'org-agenda-config)
</p>

</div>

-   **`org-roam-config.el`:** Implements a Zettelkasten system for interconnected note-taking, featuring a visual graph and database sync.

<div class="nil" id="orgf5a2875">
<p>
(use-package org-roam
    :straight (:host github :repo "org-roam/org-roam" :branch "main")
    :config (org-roam-db-autosync-mode)
    :init (setq org-roam-v2-ack t)
</p>

<p>
:custom ((org-roam-dailies-directory (file-name-concat org-roam-directory "projeto-pessoal"))
	 (org-roam-graph-link-hidden-types ("files" "https" "ref" "fuzzy")))
</p>

<p>
:bind (("C-c r f" . org-roam-node-find)
       ("C-c r c" . org-roam-capture)
       ("C-c r b" . org-roam-buffer-toggle)
       ("C-c r I" . org-roam-node-insert-immediate)
       :map org-mode-map
       ("C-c r i" . org-roam-node-insert)))
</p>

<p>
(use-package org-roam-ui
    :straight
      (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
      :after org-roam
      :custom
      ((org-roam-ui-open-on-start nil)
       (org-roam-ui-sync-theme t)
       (org-roam-ui-follow t)
       (org-roam-ui-update-on-save t))
      :init
      (org-roam-ui-mode)
      (setq org-roam-ui-latex-macros
            '(("\\Tr" . "\\mathrm{Tr}")
              ("\\tr" . "\\mathrm{Tr}")
              ("\\dyad" . "\\ket{#1}\\bra{#2}")
              ("\\order" . "\\mathcal{O}({#1})")
              ("\\I" . "\\mathbb{I}")
              ("\\norm" . "\&parallel;{#1}\&parallel;")
              ("\\id" . "\\mathbb{I}")
              ("\\expval" . "\&lang;{#1}\&rang;")
              ("\\dd" . "\\mathrm{d}")
              ("\\op" . "|{#1}\&rang;\&lang;{#2}|")
              ("\\label" . "\\vphantom")
              ("\\dv" . "\\frac{\\mathrm{d}{#1}}{\\mathrm{d}{#2}}")
              ("\\olra" . "\\overleftrightarrow{#1}"))))
</p>

<p>
(with-eval-after-load 'org-roam
  (add-to-list
   'org-roam-capture-templates
   '("r" "reference" plain
     "%?"
     :if-new
     (file+head "%&lt;%Y%m%d%H%M%S&gt;-${slug}.org"
                "#+title: ${citar-title}\n#+roam<sub>key</sub>: \({citar-citekey}\n")
     :node-properties (:ROAM_ALIASES "\){citar-citekey}")
     :unnarrowed t)))
</p>

<p>
(provide 'org-roam-config)
</p>

</div>

-   **`org-capture-config.el`:** Defines templates for quickly capturing notes, tasks, and inbox items without breaking focus.

<div class="nil" id="org43677f7">
<p>
(setq org-capture-templates
      '(("n" "Notes" entry (file+headline "~/Documents/org/notes.org" "Unsorted")
         "* UNSEEN %?\n")
        ("g" "Goals" entry (file+headline "~/Documents/org/goals.org" "Unsorted")
         "* TODO %?\n")
        ("i" "Inbox" entry (file+headline "~/Documents/org/notes.org" "Inbox")
         "* TODO &lt;%&lt;%Y-%m-%d %H:%M:%S&gt;&gt; \n %?\n")
  	("t" "Tasks" entry (file+headline "~/Documents/org/tasks.org" "Tarefas")
         "* TODO %?\n")))
(global-set-key (kbd "C-c c") 'org-capture)
(provide 'org-capture-config)
</p>

</div>

-   **`org-babel-config.el`:** Configures literate programming environments, allowing code execution within Org files for various languages.

<div class="nil" id="orgc5b9307">
<p>
(require 'org-ob-jupyter-config)
</p>

<p>
(defvar my/org-latex-babel-lang-list
  '(C jupyter shell latex fortran python ledger))
</p>

<p>
(setq org-babel-load-languages '())
(dolist (lang my/org-latex-babel-lang-list)
  (add-to-list 'org-babel-load-languages (cons lang t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)
</p>

<p>
(provide 'org-babel-config)
</p>

</div>

-   **`org-latex-config.el`:** Provides specialized tools for mathematical writing, including `cdlatex` for fast snippet expansion.

<div class="nil" id="orge96372a">
<p>
(use-package cdlatex
      :hook (org-mode . org-cdlatex-mode)
</p>

<p>
;; comandos extras
:config (add-to-list 'cdlatex-math-modify-alist '(?s "\\mathscr" nil t nil nil))
	(add-to-list 'cdlatex-math-modify-alist '(?B "\\mathbb" nil t nil nil))
	(add-to-list 'cdlatex-math-modify-alist '(?k "\\mathfrak" nil t nil nil))
	(add-to-list 'cdlatex-math-symbol-alist '(?* ("\&times;" "\&otimes;")))
	(add-to-list 'cdlatex-math-symbol-alist '(?d ("\&delta;" "\&part;" "<sup>\&dagger;</sup>")))
	(add-to-list 'cdlatex-math-symbol-alist '(?&gt; ("\&rarr;" "\\longrightarrow")))
	(add-to-list 'cdlatex-math-symbol-alist '(?. ("\&sdot;" "\o" "\max")))
	(add-to-list 'cdlatex-math-symbol-alist '(?~ ("\&asymp;" "\&cong;" "\&prop;")))
	(cdlatex-reset-mode))
</p>

<p>
(provide 'org-latex-config)
</p>

</div>

-   **`org-link-config.el`:** Customizes link behaviors for transclusion, subfigures in LaTeX, and integrated image pasting.

<div class="nil" id="org5b1bf58">
<p>
;; use subfigs in LaTeX export
</p>

<p>
;;  #+name: fig<sub>name</sub>
;;  #+attr<sub>latex</sub>: :options \centering
;;  #+caption: Figure Caption
;;  #+begin<sub>figure</sub> 
;;  <a href="subfig:img1.png">Subcaption 1 &gt;(scale=0.6)</a> 
;;  <a href="subfig:img2.png">Subcaption 2 &gt;(scale=0.6)</a>
;;  <a href="subfig:img3.png">Subcaption 3 &gt;(scale=0.6)</a>
;;  #+end<sub>figure</sub>
(with-eval-after-load 'org
  (org-link-set-parameters
   "subfig"
   :follow (lambda (file) (find-file file))
   :face '(:foreground "chocolate" :weight bold :underline t)
   :display 'full
   :export
   (lambda (file desc backend)
     (when (eq backend 'latex)
       (if (string-match "&gt;(\\(.+\\))" desc)
           (concat "\\subfigure[" (replace-regexp-in-string "\s+>(.+)" "" desc) "]"
                   "{\\includegraphics"
                   "[" (match-string 1 desc) "]"
                   "{" file "}}")
         (format "\\subfigure[%s]{\\includegraphics{%s}}" desc file))))))
</p>

<p>
;; link files and display everything as a single file
(use-package org-transclusion
  :after org
  :bind (("C-c t m" . org-transclusion-transient-menu)
	 ("C-c t t" . org-transclusion-mode)))
</p>

<p>
;; org-paste-image, simpler than org-download
(defun org-paste-image ()
  "Paste an image from the clipboard, let the user choose a directory to save it,
and insert a link to it in the buffer. Supports Org-mode and LaTeX."
  (interactive)
  (let* ((session (getenv "XDG<sub>SESSION</sub><sub>TYPE</sub>"))
         (command-header (cond
                          ((string= session "x11")
                           "xclip -selection clipboard -t image/png -o")
                          ((string= session "wayland")
                           "wl-paste &ndash;type image/png")
                          (t (error "Unknown session type: %s" session))))
         ;; choose target dir
         (base-dir (read-directory-name
                    "Choose the directory to save image: "
                    default-directory nil nil "img/"))
         (_ (make-directory base-dir t))
         (filename (concat (make-temp-name
                            (expand-file-name "img-" base-dir)) ".png"))
         (command (concat command-header " | convert - " (shell-quote-argument filename))))
</p>

<p>
(shell-command command)
(if (file-exists-p filename)
    (cond
     ((derived-mode-p 'org-mode)
      (insert (concat "<a href="" (file-relative-name filename default-directory) "">" (file-relative-name filename default-directory) "</a>"))
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
</p>

<p>
;; add <code>framed</code> to C-c C-x s // C-c C-,
(add-to-list 'org-structure-template-alist '("f" . "framed"))
</p>

<p>
(provide 'org-link-config)
</p>

</div>

-   **`org-ox-config.el`:** Manages the export engine for converting Org files into LaTeX, Reveal.js presentations, and other formats.

<div class="nil" id="org02921ae">
<p>
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
</p>

<p>
(defun my/org-export-run-on-save ()
  (interactive)
  (when (bound-and-true-p local-org-export-dispatch-last-action)
    (setq org-export-dispatch-last-action local-org-export-dispatch-last-action)
    (org-export-dispatch 1)))
</p>

<p>
(use-package org-reveal)
(use-package ox-reveal
  :after (org-reveal))
(load-library "ox-reveal")
</p>

<p>
(provide 'org-ox-config)
</p>

</div>


# Tools

General-purpose tools that integrate external applications and services directly into the Emacs workflow.

-   **`tools-ai-config.el`:** Integrates Large Language Models via `gptel`, with custom presets for coding, physics, and proofreading.

<div class="nil" id="orgc5e4b38">
<p>
;; gptel
(use-package gptel
</p>

<p>
:init (gptel-make-gemini "Gemini"
	:key (lambda ()
	       (auth-source-pick-first-password
		:host "generativelanguage.googleapis.com"
		:user "apikey"))
	:stream t)
:custom ((gptel-default-mode #'org-mode)
	 (gptel-track-media t)
	 (gptel-use-tools t)
	 (gptel-model 'gemini-flash-latest)))
</p>

<p>
(with-eval-after-load 'gptel
  (require 'tools-ai-presets-config))
</p>

<p>
(use-package gptel-commit
  :straight (:host github :repo "lakkiy/gptel-commit")
  :after (gptel magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . gptel-commit)))
</p>

<p>
(use-package gptel-agent
:config (gptel-agent-update))
</p>

<p>
(use-package gptel-org-tools
  :straight (:host codeberg :repo "bajsicki/gptel-got" :branch "main"))
</p>

<p>
(use-package ragmacs
   :straight (:host github :repo "positron-solutions/ragmacs":branch "master")
   :after gptel)
</p>

<p>
(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :config (mapcar (apply-partially #'apply #'gptel-make-tool)
		  (llm-tool-collection-get-all)))
</p>

<p>
(provide 'tools-ai-config)
</p>

</div>

-   **`tools-completion-config.el`:** Configures `corfu` for modern in-buffer completion and `eglot` for Language Server Protocol (LSP) support.

<div class="nil" id="orgc4bcda6">
<p>
;; corfu para auto-complete
(use-package corfu
  :init (global-corfu-mode))
(provide 'tools-completion-config)
</p>

<p>
;; eglot para LSP
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (javascript-mode . eglot-ensure)
  (julia-mode . eglot-ensure)
  (java-mode . eglot-ensure))
</p>

<p>
(add-to-list 'exec-path "<i>home/nicolas</i>.local/bin")
(use-package eglot-booster
  :after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster" :branch "main")
  :config (eglot-booster-mode))
</p>

<p>
;; (use-package json-rpc)
</p>

</div>

-   **`tools-references-config.el`:** Manages academic bibliographies with `citar` and `ebib`, integrating PDFs and citations into the workflow.

<div class="nil" id="org068ca20">
<p>
;; i'm used to using <code>org-ref</code>, but I'm going to migrate to <code>citar</code>. 
;; the old config is going to be here
</p>

<p>
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
</p>

<p>
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
</p>

<p>
(use-package citar
  :custom
  (citar-bibliography (list my/bibliography-file))
  (citar-notes-paths (list org-roam-directory))
  (citar-library-paths (list my/pdf-library))
  :hook ((LaTeX-mode . citar-capf-setup)
	 (org-mode . citar-capf-setup)))
</p>

<p>
;; not sure if this should be on
(use-package citar-org-roam
  :after (citar org-roam)
  :custom (citar-org-roam-capture-template-key "r"))
</p>

<p>
(citar-org-roam-mode)
</p>

<p>
(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))
</p>

<p>
(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my/bibliography-file)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path my/pdf-library))
</p>

<p>
;; useful to look for papers, never really used it
(use-package biblio
  :demand t)
</p>

<p>
(use-package biblio-openlibrary
  :straight (:host github :repo "fabcontigiani/biblio-openlibrary" :branch "master"))
(use-package biblio-gbooks
  :straight (:host github :repo "jrasband/biblio-gbooks" :branch "main"))
</p>

<p>
(defun my/ebib-reading-list-add-org-cite ()
  "Add an Org-cite citation to the newly created Ebib reading-list item."
  (let ((key (ebib&ndash;get-key-at-point)))
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
        (insert (format "- (??, ????)\n" key)))
      (save-buffer))))
</p>

<p>
;; bibliography tool
(use-package ebib
  :after biblio
</p>

<p>
:custom ((ebib-default-directory my/library-directory)
	 (ebib-bib-search-dirs (file-name-concat my/library-directory "pdfs"))
	 (ebib-preload-bib-files `(,my/bibliography-file))
	 (ebib-reading-list-file (file-name-concat ebib-default-directory "reading-list.org")))
:config
(require 'ebib-biblio)
(define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
(define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import))
</p>

<p>
(add-hook 'ebib-reading-list-new-item-hook #'my/ebib-reading-list-add-org-cite)
</p>

<p>
(provide 'tools-references-config)
</p>

</div>

-   **`tools-utils-config.el`:** Houses essential utilities like Magit for Git, PDF-Tools for document viewing, and Jinx for spellchecking.

<div class="nil" id="org04ed0c9">
<p>
;; box instead of dedicated buffer
(use-package eldoc-box)
</p>

<p>
;; expand-region increases region by semantic expressions
(use-package expand-region
    :bind ("C-=" . er/expand-region))
</p>

<p>
;; hide-show-mode folds code
;; in evil:
;; - zr (show-all)
;; - zm (fold-all)
;; - zo (show-this)
;; - zc (fold-this)
(add-hook 'prog-mode-hook 'hs-minor-mode)
</p>

<p>
;; elfeed
(use-package elfeed
  :custom (elfeed-db-directory "~/.local/share/elfeed")
  :bind ("C-c e" . elfeed))
</p>

<p>
(use-package elfeed-org
  :straight (:host github :repo "remyhonig/elfeed-org" :branch "master")
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (list (file-name-concat org-directory "elfeed.org"))))
</p>

<p>
;;jinx
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :init (setq jinx-languages "en<sub>US</sub> pt<sub>BR</sub>")
  :bind (("M-\(" . jinx-correct)
         ("C-M-\)" . jinx-languages)))
(add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
;;ledger
(use-package ledger-mode
  :custom
  ((ledger-reports
    (quote
     (("bal cf" "%(binary) -f %(ledger-file) bal ^Income ^Expenses &ndash;real")
      ("bal nw" "%(binary) -f %(ledger-file) bal ^Assets ^Liabilities &ndash;real")
      ("bal precos" "%(binary) -f %(ledger-file) &ndash;price-db btc<sub>price.db</sub> &ndash;price-db comm.db -V bal")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("reg - price" "%(binary) -f %(ledger-file) reg -V &ndash;price-db btc<sub>price.db</sub> &ndash;price-db comm.db")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
   (ledger-post-amount-alignment-column 60))
  :hook
  (ledger-mode . (lambda ()
                   (setq-local tab-always-indent 'complete)
                   (setq-local completion-ignore-case t)
                   (setq-local ledger-complete-in-steps t))))
</p>

<p>
;; magit
(use-package magit
  :bind ("C-x g" . magit-status))
</p>

<p>
(use-package forge)
</p>

<p>
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
</p>

<p>
;; pdf-tools
(use-package pdf-tools
  :hook (pdf-view-mode . blink-cursor-mode)
</p>

<p>
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
</p>

<p>
:mode ("\\.pdf" . pdf-view-mode)
</p>

<p>
:custom (pdf-view-midnight-colors '("#FFBB33" . "#222222")))
</p>

<p>
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
</p>

<p>
;; terminal
;; I was using <code>toggle-term</code> to access popup terminal, but it was kinda clunky
;; apparently there's a 'popper' thingy to recover such behavior
(use-package popper
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("<b>vterm</b>"
	  "<b>eshell</b>"
	  "\\*Messages\\*"
	  "\\*Warning\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))
</p>

<p>
(use-package vterm)
</p>

<p>
;;vundo
(use-package vundo
  :demand t)
</p>

<p>
;; winner mode allows C-c &lt;left&gt; / C-c &lt;right&gt; to recover previous window configuration
(winner-mode)
</p>

<p>
;; tree-sitter
(use-package tree-sitter
  :custom (treesit-language-source-alist
	   '((python "<a href="https://github.com/tree-sitter/tree-sitter-python">https://github.com/tree-sitter/tree-sitter-python</a>")
             (clojure  "<a href="https://github.com/tree-sitter/tree-sitter-clojure">https://github.com/tree-sitter/tree-sitter-clojure</a>")
             (julia  "<a href="https://github.com/tree-sitter/tree-sitter-julia">https://github.com/tree-sitter/tree-sitter-julia</a>")
	     (bash   "<a href="https://github.com/tree-sitter/tree-sitter-bash">https://github.com/tree-sitter/tree-sitter-bash</a>")
             (c      "<a href="https://github.com/tree-sitter/tree-sitter-c">https://github.com/tree-sitter/tree-sitter-c</a>")
             (cpp    "<a href="https://github.com/tree-sitter/tree-sitter-cpp">https://github.com/tree-sitter/tree-sitter-cpp</a>"))))
</p>

<p>
;; split and follow window
(defun my/split-and-follow (direction)
  (interactive "c")
  (catch 'my-tag
    (cond ((= direction ?h) (split-window-below))
  	((= direction ?v) (split-window-right))
  	(t (throw 'my-tag "no correct character pressed")))
    (other-window 1)))
</p>

<p>
(defun my/split-and-follow-horizontally ()
  (interactive)
  (my/split-and-follow ?h))
</p>

<p>
(defun my/split-and-follow-vertically ()
  (interactive)
  (my/split-and-follow ?v))
</p>

<p>
;; zoom-window, just like &lt;leader&gt;-z on tmux
(use-package zoom-window
  :bind (("M-z" . zoom-window-zoom)
	 ("C-M-z" . zoom-window-next)))
</p>

<p>
;; (defvar zoom-register ?z
;;   "the register to store the window configuration for zooming/unzooming.")
</p>

<p>
;; (defvar zoomed-in-p nil
;;   "a flag to track if the window is currently zoomed in.")
</p>

<p>
;; (defun toggle-zoom-window ()
;;   "toggle zooming the current window: maximize or restore."
;;   (interactive)
;;   (if zoomed-in-p
;;       (restore-window-configuration-from-zoom-register)
;;     (save-window-configuration-to-zoom-register)
;;     (delete-other-windows))
;;   (setq zoomed-in-p (not zoomed-in-p)))
</p>

<p>
;; (defun save-window-configuration-to-zoom-register ()
;;   "save the current window configuration to a register for zooming/unzooming."
;;   (window-configuration-to-register zoom-register)
;;   (message "Window configuration saved for zooming."))
</p>

<p>
;; (defun restore-window-configuration-from-zoom-register ()
;;   "restore the window configuration from the zoom register."
;;   (jump-to-register zoom-register)
;;   (message "Window configuration restored."))
</p>

<p>
;; (global-set-key (kbd "M-z") 'toggle-zoom-window)
</p>

<p>
(provide 'tools-utils-config)
</p>

</div>


# Programming Languages

Language-specific configurations that provide specialized editing features, REPLs, and formatting tools.

-   **`langs-python-config.el`:** Python setup including virtual environment management and black formatting.

<div class="nil" id="org912ac8a">
<p>
(use-package python-mode
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))
</p>

<p>
(use-package python-black)
(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode))
</p>

<p>
(provide 'langs-python-config)
</p>

</div>

-   **`langs-julia-config.el`:** Comprehensive Julia support with an integrated REPL and specialized keybindings.

<div class="nil" id="orgd187cd7">
<p>
(use-package julia-repl)
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :interpreter ("julia" . julia-mode)
  :bind
  (:map julia-mode-map
        ("C-c C-z" . julia-repl)
        ("C-c C-c" . julia-repl-send-buffer)
        ("C-&lt;return&gt;" . julia-repl-send-region-or-line)
        ("C-&lt;enter&gt;" . julia-repl-send-region-or-line)
        ("C-c C-d" . julia-repl-doc)))
</p>

<p>
(provide 'langs-julia-config)
</p>

</div>

-   **`langs-latex-config.el`:** Full LaTeX environment using AUCTeX, Synctex, and mathematical calculation helpers.

<div class="nil" id="org0e8c7a9">
<p>
;; auctex enhances usability in latex
(use-package auctex
  :init (require 'latex)
  :custom ((TeX-source-correlate-mode t)
	   (TeX-source-correlate-method-active 'synctex))
  :bind (:map LaTeX-mode-map
	      ("C-S-e" . latex-math-from-calc)
  	      :map org-mode-map
	      ("C-S-e" . latex-math-from-calc))
</p>

<p>
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
</p>

<p>
(use-package latex-extra)
</p>

<p>
;; latex references manager &ndash; not really using latex much tho
(use-package reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (reftex-default-bibliography (list my/bibliography-file))
  :hook ((TeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . LaTeX-math-mode)
	 (TeX-mode . outline-minor-mode)
	 (TeX-mode . turn-on-reftex)))
</p>

<p>
(provide 'langs-latex-config)
</p>

</div>

-   **`langs-clojure-config.el`:** Clojure development tools utilizing Tree-sitter and the CIDER environment.

<div class="nil" id="org11759c1">
<p>
(use-package clojure-ts-mode)
</p>

<p>
(use-package cider)
(provide 'langs-clojure-config)
</p>

</div>

