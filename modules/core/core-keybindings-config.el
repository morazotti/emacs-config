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
