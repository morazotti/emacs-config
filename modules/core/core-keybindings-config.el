(global-set-key (kbd "<f5>") 'revert-buffer)

;; vim-keybindings
(use-package evil
  :init
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
  "bb" 'ibuffer
  "bk" 'kill-buffer
  "bs" 'save-buffer
  "." 'dired
  "z" 'zoom-window-zoom

  ;; commands
  "x" 'execute-extended-command
  ":" 'eval-expression
  ";" 'avy-goto-line

  ;; movement
  "TAB" 'other-window
  "s" 'avy-goto-char
  "/" 'consult-line

  ;; ispell
  "t." 'flyspell-auto-correct-word
  "tb" 'ispell-buffer
  "tw" 'ispell-word

  ;; ;; windows
  ;; "w0" 'delete-window
  ;; "w1" 'delete-other-windows
  ;; "w2" 'split-and-follow-horizontally
  ;; "w3" 'split-and-follow-vertically
  ;; "wp" 'winner-undo
  ;; "wn" 'winner-redo
  ;; "w=" 'balance-windows
  ;; "wt" 'transpose-frame
  ;; "w TAB" 'ace-window

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
  "cd" 'consult-find
  ;; "cD" 'consult-locate
  "cg" 'consult-grep
  "cG" 'consult-git-grep
  "cr" 'consult-ripgrep

  ;; org ref
  ;; "[" 'ivy-bibtex
  ;; "]" 'org-ref-insert-link
  ;; ")" 'org-ref-insert-ref-link

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

  ;; undo-tree
  "u" 'vundo

  ;; elfeed
  "ee" 'elfeed

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
