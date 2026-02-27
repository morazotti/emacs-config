;; box instead of dedicated buffer
(use-package posframe)
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

;; casual
;; adds transient menu to calc
(use-package casual
  :bind (:map calc-mode-map
	      ("C-c C-c" . casual-calc-tmenu)
	 :map dired-mode-map
	      ("C-c C-c" . casual-dired-tmenu)))

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

;; nutrition
(use-package org-nutrition
  :config (require 'org-nutrition-taco)
  :straight (:host github :repo "morazotti/org-nutrition" :branch "master"))

;; pdf-tools
(use-package pdf-tools
  :hook (pdf-view-mode . blink-cursor-mode)
  :bind
  (:map pdf-view-mode-map
	(":" . 'pdf-view-goto-page)
	("C-s" . 'isearch-forward)
	("C-r" . 'isearch-backward)
	("/" . 'isearch-forward)
	("?" . 'isearch-backward)
	("j" . 'pdf-view-next-line-or-next-page)
	("k" . 'pdf-view-previous-line-or-previous-page)
	("l" . 'image-forward-hscroll)
	("h" . 'image-backward-hscroll)
	("J" . 'pdf-view-next-page-command)
	("K" . 'pdf-view-previous-page-command)
	("TAB" . 'pdf-outline)
	("C-S-r" . 'pdf-view-midnight-minor-mode)
	("T" . 'pdf-view-themed-minor-mode)
	("s" . 'pdf-view-fit-width-to-window)
	("a" . 'pdf-view-fit-page-to-window)
	("o" . 'pdf-occur)
	("<C-mouse-4>" . #'pdf-view-enlarge)
	("<C-mouse-5>" . #'pdf-view-shrink))

  :config
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
    (cond
     ((= direction ?h) (split-window-below))
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


(provide 'tools-utils-config)
